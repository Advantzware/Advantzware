&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : Jasper/spJasper.p
    Purpose     : SUPER-PROCEDURE Jasper Functions and Procedures

    Syntax      : RUN Jasper/spJasper.p

    Description : Jasper Functions & Procedures

    Author(s)   : Ron Stark
    Created     : 12.5.2018
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
&SCOPED-DEFINE aoaJasper 6
&SCOPED-DEFINE aoaJasperGap 5
&SCOPED-DEFINE noBrowseRefresh

DEFINE VARIABLE aoaBatchSeq         AS INTEGER   NO-UNDO.
DEFINE VARIABLE aoaCompany          AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaProgramID        AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaTitle            AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaUserID           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJasperFile         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSelectedColumns    AS CHARACTER NO-UNDO.
DEFINE VARIABLE hAppSrv             AS HANDLE    NO-UNDO.
DEFINE VARIABLE hAppSrvBin          AS HANDLE    NO-UNDO.
DEFINE VARIABLE iIndent             AS INTEGER   NO-UNDO.
DEFINE VARIABLE lDetailDataSet      AS LOGICAL   NO-UNDO EXTENT 10.
DEFINE VARIABLE lJasperStarter      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lSummaryDataSet     AS LOGICAL   NO-UNDO EXTENT 10.
DEFINE VARIABLE lUseDefault         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE svShowParameters    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE svShowReportHeader  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE svShowReportFooter  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE svShowPageHeader    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE svShowPageFooter    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE svShowGroupHeader   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE svShowGroupFooter   AS LOGICAL   NO-UNDO.

DEFINE BUFFER jasperUserPrint FOR user-print.

DEFINE TEMP-TABLE ttTaskFile NO-UNDO
    FIELD taskFile AS CHARACTER
    .

{AOA/includes/ttColumn.i}
{AOA/includes/aoaProcedures.i}
{AOA/includes/pRunBusinessLogic.i}

DEFINE STREAM sLocalCSV.
DEFINE STREAM sJasper.
DEFINE STREAM sTemplate.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fDataType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fDataType Procedure
FUNCTION fDataType RETURNS CHARACTER 
  (ipcDataType AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-fFormatValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fFormatValue Procedure 
FUNCTION fFormatValue RETURNS CHARACTER
  (iphTable AS HANDLE, ipcField AS CHARACTER,
   ipcFormat AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fGetModule) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetModule Procedure 
FUNCTION fGetModule RETURNS CHARACTER
  ( ipcType AS CHARACTER, ipcProgramID AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fJasperCalcPattern) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fJasperCalcPattern Procedure 
FUNCTION fJasperCalcPattern RETURNS CHARACTER
  (ipcDataType AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fJasperPattern) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fJasperPattern Procedure 
FUNCTION fJasperPattern RETURNS CHARACTER
  (ipcFormat AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fJasperReportSize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fJasperReportSize Procedure 
FUNCTION fJasperReportSize RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 

lJasperStarter = INDEX(OS-GETENV("Path"),"jasperstarter") NE 0.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pClearTaskFiles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pClearTaskFiles Procedure
PROCEDURE pClearTaskFiles:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttTaskFile.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pCreateDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateDir Procedure
PROCEDURE pCreateDir:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcUserFolder   AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cJasperFolder AS CHARACTER NO-UNDO.

    /* ensure needed folders exist */
    OS-CREATE-DIR "TaskResults".
    OS-CREATE-DIR "users".
    opcUserFolder = "users/" + aoaUserID + "/".
    OS-CREATE-DIR VALUE(opcUserFolder).
    cJasperFolder = "users/" + aoaUserID + "/Jasper/".
    OS-CREATE-DIR VALUE(cJasperFolder).

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCreateTaskFileRecord) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateTaskFileRecord Procedure
PROCEDURE pCreateTaskFileRecord:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTaskFile AS CHARACTER NO-UNDO.

    IF NOT CAN-FIND(FIRST ttTaskFile
                    WHERE ttTaskFile.taskFile EQ ipcTaskFile) THEN DO:
        CREATE ttTaskFile.
        ttTaskFile.taskFile = ipcTaskFile.
    END. /* if not can-find */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pDeleteSessionParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDeleteSessionParam Procedure
PROCEDURE pDeleteSessionParam:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.

    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTables AS INTEGER NO-UNDO.

    RUN spGetSessionParam (ipcType + "Tables", OUTPUT iTables).
    DO idx = 1 TO iTables:
        RUN spDeleteSessionParam (ipcType + "Handle" + STRING(idx)).
        RUN spDeleteSessionParam (ipcType + "Title"  + STRING(idx)).
    END. /* do idx */
    RUN spDeleteSessionParam (ipcType + "Tables").

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pGetSelectedColumns) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSelectedColumns Procedure 
PROCEDURE pGetSelectedColumns :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iColumn AS INTEGER NO-UNDO INITIAL 1.
    
    DEFINE BUFFER ttColumn FOR ttColumn.
    
    cSelectedColumns = "".
    FOR EACH ttColumn
        WHERE ttColumn.isActive EQ YES
           BY ttColumn.ttOrder
        :
        ASSIGN
            cSelectedColumns        = cSelectedColumns + ttColumn.ttField + ","
            ttColumn.ttJasperSize   = INTEGER(ttColumn.ttSize * {&aoaJasper})
            ttColumn.ttJasperColumn = iColumn
            iColumn = iColumn + ttColumn.ttJasperSize + {&aoaJasperGap}
            .
    END. /* each ttColumn */
    cSelectedColumns = TRIM(cSelectedColumns,",").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pGetUserParamValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetUserParamValue Procedure
PROCEDURE pGetUserParamValue:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprRowID AS ROWID NO-UNDO.

    DEFINE VARIABLE cField AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTable AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTemp  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dSize  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE hTable AS HANDLE    NO-UNDO.
    DEFINE VARIABLE dWidth AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE jdx    AS INTEGER   NO-UNDO.

    FIND FIRST dynParamValue NO-LOCK WHERE ROWID(dynParamValue) EQ iprRowID.
    FIND FIRST dynSubject NO-LOCK
         WHERE dynSubject.subjectID EQ dynParamValue.subjectID
         NO-ERROR.
    IF AVAILABLE dynSubject AND dynSubject.businessLogic NE "" THEN DO:
        RUN VALUE(dynSubject.businessLogic) PERSISTENT SET hBusinessLogic.
        ASSIGN
            hTable = DYNAMIC-FUNCTION('fGetTableHandle' IN hBusinessLogic)
            hTable = hTable:DEFAULT-BUFFER-HANDLE
            .
    END.
    ASSIGN
        aoaProgramID = dynParamValue.prgmName
        aoaUserID    = dynParamValue.user-id
        aoaBatchSeq  = dynParamValue.paramValueID
        .
    FOR EACH dynValueParam NO-LOCK
        WHERE dynValueParam.subjectID    EQ dynParamValue.subjectID
          AND dynValueParam.user-id      EQ dynParamValue.user-id
          AND dynValueParam.prgmName     EQ dynParamValue.prgmName
          AND dynValueParam.paramValueID EQ dynParamValue.paramValueID
        :
        CASE dynValueParam.paramName:
            WHEN "svShowParameters" THEN
            svShowParameters   = dynValueParam.paramValue EQ "yes".
            WHEN "svShowReportHeader" THEN
            svShowReportHeader = dynValueParam.paramValue EQ "yes".
            WHEN "svShowReportFooter" THEN
            svShowReportFooter = dynValueParam.paramValue EQ "yes".
            WHEN "svShowPageHeader" THEN
            svShowPageHeader   = dynValueParam.paramValue EQ "yes".
            WHEN "svShowPageFooter" THEN
            svShowPageFooter   = dynValueParam.paramValue EQ "yes".
            WHEN "svShowGroupHeader" THEN
            svShowGroupHeader  = dynValueParam.paramValue EQ "yes".
            WHEN "svShowGroupFooter" THEN
            svShowGroupFooter  = dynValueParam.paramValue EQ "yes".
        END CASE.
    END. /* each dynvalueparam */
    EMPTY TEMP-TABLE ttColumn.
    EMPTY TEMP-TABLE ttGroupCalc.
    FOR EACH dynValueColumn NO-LOCK
        WHERE dynValueColumn.subjectID    EQ dynParamValue.subjectID
          AND dynValueColumn.user-id      EQ dynParamValue.user-id
          AND dynValueColumn.prgmName     EQ dynParamValue.prgmName
          AND dynValueColumn.paramValueID EQ dynParamValue.paramValueID
           BY dynValueColumn.sortOrder
        :
        dWidth = 10.
        IF NUM-ENTRIES(dynValueColumn.colName,".") EQ 2 THEN DO:
            ASSIGN
                cTable = ENTRY(1,dynValueColumn.colName,".")
                cField = ENTRY(2,dynValueColumn.colName,".")
                .
            IF dynSubject.businessLogic EQ "" THEN DO:
                cTemp = cField.
                IF INDEX(cTemp,"[") NE 0 THEN
                cTemp = SUBSTRING(cTemp,1,INDEX(cTemp,"[") - 1).
                IF cTable NE "ttUDF" THEN DO:
                    CREATE BUFFER hTable FOR TABLE cTable.
                    dWidth = hTable:BUFFER-FIELD(cTemp):WIDTH.
                END. /* if ctable */
            END. /* if not business logic */
            ELSE
            IF VALID-HANDLE(hTable) THEN
            dWidth = hTable:BUFFER-FIELD(cField):WIDTH.
        END. /* if table.field */
        ELSE
        cField = dynValueColumn.colName.
        IF dynValueColumn.dataType EQ "String" THEN
        dSize = FONT-TABLE:GET-TEXT-WIDTH-CHARS(STRING(FILL("X",256),dynValueColumn.colFormat),0).
        ELSE
        dSize = MAX(dWidth,LENGTH(dynValueColumn.colLabel)).
        RUN pCreatettColumn (
            cTable,
            cField,
            dynValueColumn.sortOrder,
            dynValueColumn.isActive,
            dynValueColumn.colLabel,
            dynValueColumn.dataType,
            dynValueColumn.colFormat,
            dWidth,
            dSize,
            dynValueColumn.calcFormula
            ).
        ASSIGN
            ttColumn.isGroup      = dynValueColumn.isGroup
            ttColumn.ttGroupLabel = dynValueColumn.groupLabel
            .
        IF dynValueColumn.groupCalc NE "" THEN DO:
            DO jdx = 1 TO NUM-ENTRIES(dynValueColumn.groupCalc) BY 2:
                CREATE ttGroupCalc.
                ASSIGN 
                    ttGroupCalc.ttField    = cField
                    ttGroupCalc.ttGroup    = ENTRY(jdx,dynValueColumn.groupCalc)
                    ttGroupCalc.ttCalcType = ENTRY(jdx + 1,dynValueColumn.groupCalc)
                    .
            END. /* do jdx */
            ttColumn.ttGroupCalc = fJasperGroupCalc(ttColumn.ttField).
        END. /* if field-value */
    END. /* each dynvaluecolumn */
    RELEASE dynSubject.
   
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pGetUserPrint) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetUserPrint Procedure 
PROCEDURE pGetUserPrint :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprRowID AS ROWID NO-UNDO.
    
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    FIND user-print NO-LOCK WHERE ROWID(user-print) EQ iprRowID.
    ASSIGN
        aoaCompany   = user-print.company
        aoaProgramID = user-print.program-id
        aoaUserID    = user-print.user-id
        aoaBatchSeq  = user-print.batch-seq
        .
    DO idx = 1 TO EXTENT(user-print.field-name):
        IF user-print.field-name[idx] EQ "" THEN LEAVE.
        CASE user-print.field-name[idx]:
            WHEN "svSelectedColumns" THEN
            cSelectedColumns   = user-print.field-value[idx].
            WHEN "svTitle" THEN
            aoaTitle           = user-print.field-value[idx].
            WHEN "svShowParameters" THEN
            svShowParameters   = user-print.field-value[idx] EQ "yes".
            WHEN "svShowReportHeader" THEN
            svShowReportHeader = user-print.field-value[idx] EQ "yes".
            WHEN "svShowReportFooter" THEN
            svShowReportFooter = user-print.field-value[idx] EQ "yes".
            WHEN "svShowPageHeader" THEN
            svShowPageHeader   = user-print.field-value[idx] EQ "yes".
            WHEN "svShowPageFooter" THEN
            svShowPageFooter   = user-print.field-value[idx] EQ "yes".
            WHEN "svShowGroupHeader" THEN
            svShowGroupHeader  = user-print.field-value[idx] EQ "yes".
            WHEN "svShowGroupFooter" THEN
            svShowGroupFooter  = user-print.field-value[idx] EQ "yes".
        END CASE.
    END. /* do idx */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pJasperBackgroundBand) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperBackgroundBand Procedure 
PROCEDURE pJasperBackgroundBand :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* background band */
    PUT STREAM sJasper UNFORMATTED
        "    <background>" SKIP
        "        <band splitType=~"Stretch~"/>" SKIP
        "    </background>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pJasperBoxStyle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperBoxStyle Procedure
PROCEDURE pJasperBoxStyle:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcType     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiNumber   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcElements AS CHARACTER NO-UNDO.

    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    DO idx = 1 TO NUM-ENTRIES(ipcElements):
        PUT STREAM sJasper UNFORMATTED
            "    <style name=~"" ipcType ipiNumber "_" ENTRY(idx,ipcElements) "~" mode=~"Opaque~" backcolor=~""
            (IF idx EQ 1 THEN "#BFE1FF" ELSE IF idx EQ 2 THEN "#F0F8FF" ELSE "#FFFFFF") "~">" SKIP
            "        <box>" SKIP
            "            <pen lineWidth=~"0.5~" lineColor=~"#000000~"/>" SKIP
            "            <topPen lineWidth=~"0.5~" lineColor=~"#000000~"/>" SKIP
            "            <leftPen lineWidth=~"0.5~" lineColor=~"#000000~"/>" SKIP
            "            <bottomPen lineWidth=~"0.5~" lineColor=~"#000000~"/>" SKIP
            "            <rightPen lineWidth=~"0.5~" lineColor=~"#000000~"/>" SKIP
            "        </box>" SKIP
            "    </style>" SKIP
            .
    END. /* do idx */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pJasperColumnFooterBand) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperColumnFooterBand Procedure 
PROCEDURE pJasperColumnFooterBand :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* column footer band */
    PUT STREAM sJasper UNFORMATTED
        "    <columnFooter>" SKIP
        "        <band height=~"" 14 "~" splitType=~"Stretch~">" SKIP
        .
    RUN pJasperGroupType ("Column").
    PUT STREAM sJasper UNFORMATTED
        "        </band>" SKIP
        "    </columnFooter>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pJasperColumnHeaderBand) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperColumnHeaderBand Procedure 
PROCEDURE pJasperColumnHeaderBand :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/    
    DEFINE BUFFER ttColumn FOR ttColumn.
    
    /* column header band */
    PUT STREAM sJasper UNFORMATTED
        "    <columnHeader>" SKIP
        "        <band height=~"" 14 "~" splitType=~"Stretch~">" SKIP
        .
    FOR EACH ttColumn
        WHERE ttColumn.isActive EQ YES
           BY ttColumn.ttOrder
        :
        PUT STREAM sJasper UNFORMATTED
            "            <staticText>" SKIP
            "                <reportElement "
            "x=~"" ttColumn.ttJasperColumn "~" "
            "y=~"" 0 "~" "
            "width=~"" ttColumn.ttJasperSize "~" "
            "height=~"" 14 "~"/>" SKIP
            "                    <textElement"
            .
        IF CAN-DO("Decimal,Integer",ttColumn.ttType) THEN
        PUT STREAM sJasper UNFORMATTED
            " textAlignment=~"Right~""
            .
        PUT STREAM sJasper UNFORMATTED
            ">" SKIP
            "                        <font isBold=~"true~" isUnderline=~"true~"/>" SKIP
            "                    </textElement>" SKIP
            "                <text><![CDATA[" ttColumn.ttLabel "]]></text>" SKIP
            "            </staticText>" SKIP
            .
        END. /* each ttColumn */
    PUT STREAM sJasper UNFORMATTED
        "        </band>" SKIP
        "    </columnHeader>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pJasperCopy) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperCopy Procedure 
PROCEDURE pJasperCopy :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcJasperFile AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cJasperFile AS CHARACTER NO-UNDO.
    
    IF USERID("ASI") NE "NoSweat" THEN RETURN.
    
    cJasperFile = REPLACE(
        ipcJasperFile,
        "users\" + USERID("ASI"),
        "C:\Users\RStark\JaspersoftWorkspace\MyReports"
        ). 
    OS-COPY VALUE(ipcJasperFile) VALUE(cJasperFile).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pJasperDetailBand) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperDetailBand Procedure 
PROCEDURE pJasperDetailBand :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiSize AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTables    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTables    AS INTEGER   NO-UNDO.

    DEFINE BUFFER ttColumn FOR ttColumn.
    
    /* detail band */
    DO idx = 1 TO EXTENT(lDetailDataSet):
        IF lDetailDataSet[idx] THEN
        iTables = iTables + 1.
    END. /* do itables */
    PUT STREAM sJasper UNFORMATTED
        "    <detail>" SKIP
        "        <band height=~"" iTables * 42 + 14 + (IF iTables NE 0 THEN 4 ELSE 0) "~" splitType=~"Stretch~">" SKIP
        "            <rectangle radius=~"" 0 "~">" SKIP
        "                <reportElement style=~"Zebra~" mode=~"Opaque~" "
        "x=~"" 0 "~" "
        "y=~"" 0 "~" "
        "width=~"" ipiSize - 40 "~" "
        "height=~"" 14 "~"/>" SKIP
        "                <graphicElement>" SKIP
        "                    <pen lineWidth=~"0.0~"/>" SKIP
        "                </graphicElement>" SKIP
        "            </rectangle>" SKIP
        "            <textField isBlankWhenNull=~"true~">" SKIP
        "                <reportElement x=~"1~" y=~"0~" width=~"350~" height=~"14~">" SKIP
        "                    <property name=~"com.jaspersoft.studio.spreadsheet.connectionID~"/>" SKIP
        "                </reportElement>" SKIP
        "                <textFieldExpression><![CDATA[$F~{NoDataMessage}]]></textFieldExpression>" SKIP
        "            </textField>" SKIP
        .
    FOR EACH ttColumn
        WHERE ttColumn.isActive EQ YES
           BY ttColumn.ttOrder
        :
        PUT STREAM sJasper UNFORMATTED
            "            <textField isBlankWhenNull=~"true~""
            .
        IF CAN-DO("Decimal,Integer",ttColumn.ttType) THEN
        PUT STREAM sJasper UNFORMATTED
            " pattern=~"" fJasperPattern(ttColumn.ttFormat) "~""
            .
        PUT STREAM sJasper UNFORMATTED
            ">" SKIP
            "                <reportElement "
            "x=~"" ttColumn.ttJasperColumn "~" "
            "y=~"" 0 "~" "
            "width=~"" ttColumn.ttJasperSize "~" "
            "height=~"" 14 "~">" SKIP
            "                    <property name=~"com.jaspersoft.studio.spreadsheet.connectionID~"/>" SKIP
            "                </reportElement>" SKIP
            .
        IF CAN-DO("Decimal,Integer",ttColumn.ttType) THEN
        PUT STREAM sJasper UNFORMATTED
            "                <textElement textAlignment=~"Right~"/>" SKIP
            .
        IF ttColumn.ttFormula NE "" AND INDEX(ttColumn.ttFormula,"|") EQ 0 THEN
        cFieldName = ttColumn.ttFormula.
        ELSE
        ASSIGN
            cFieldName = (IF ttColumn.ttTable NE "" AND NOT ttColumn.ttField BEGINS "Calc" THEN ttColumn.ttTable + "__" ELSE "")
                       + ttColumn.ttField
            cFieldName = REPLACE(cFieldName,"[","")
            cFieldName = REPLACE(cFieldName,"]","")
            cFieldName = "$F~{" + cFieldName + "}"
            . 
        PUT STREAM sJasper UNFORMATTED
            "                <textFieldExpression><![CDATA[" cFieldName
            "]]></textFieldExpression>" SKIP
            "            </textField>" SKIP
            .
    END. /* each ttColumn */
    RUN pJasperSubDataSet ("Detail", "ComponentElement").
    PUT STREAM sJasper UNFORMATTED
        "        </band>" SKIP
        "    </detail>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pJasperFieldDeclarations) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperFieldDeclarations Procedure 
PROCEDURE pJasperFieldDeclarations :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cData      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDataType  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTables    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTypes     AS CHARACTER NO-UNDO INITIAL "Detail,Summary".
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTables    AS INTEGER   NO-UNDO.

    DEFINE BUFFER ttColumn FOR ttColumn.
    
    DO idx = 1 TO NUM-ENTRIES(cTypes):
        RUN spGetSessionParam (ENTRY(idx,cTypes) + "Tables", OUTPUT cTables).
        iTables = INTEGER(cTables).
        IF iTables EQ 0 THEN NEXT.
        DO jdx = 1 TO iTables:
            RUN pJasperBoxStyle (
                ENTRY(idx,cTypes),
                jdx,
                IF ENTRY(idx,cTypes) EQ "Summary" THEN "TH,CH,TD" ELSE "CH,TD"
                ).
        END. /* do jdx */
    END. /* do idx */
    iIndent = 8.
    DO idx = 1 TO NUM-ENTRIES(cTypes):
        RUN pJasperSubDataSet (ENTRY(idx,cTypes), "FieldDeclarations").
    END. /* do idx */
    iIndent = 4.
    PUT STREAM sJasper UNFORMATTED
            "    <parameter name=~"paramRecordID~" class=~"java.lang.String~" isForPrompting=~"false~"/>" SKIP
            .
    RUN pJasperQueryString (REPLACE(aoaTitle," ","_"), REPLACE(aoaTitle," ","")).
    RUN pJasperFieldDeclarationsPut ("NoDataMessage", "String", "NoDataMessage").
    FOR EACH ttColumn
          BY ttColumn.ttOrder
        :
        ASSIGN
            cDataType  = fDataType(ttColumn.ttType)
            cFieldName = (IF ttColumn.ttTable NE "" AND NOT ttColumn.ttField BEGINS "Calc" THEN ttColumn.ttTable + "__" ELSE "")
                       + ttColumn.ttField
            cFieldName = REPLACE(cFieldName,"[","")
            cFieldName = REPLACE(cFieldName,"]","")
            cData      = IF ttColumn.ttFormula NE "" AND INDEX(ttColumn.ttFormula,"|") EQ 0 THEN ttColumn.ttFormula
                         ELSE cFieldName
            .
        RUN pJasperFieldDeclarationsPut (cFieldName, cDataType, cData).
    END. /* each ttColumn */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pJasperFieldDeclarationsPut) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperFieldDeclarationsPut Procedure
PROCEDURE pJasperFieldDeclarationsPut:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFieldName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDataType  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcData      AS CHARACTER NO-UNDO.

    PUT STREAM sJasper UNFORMATTED
        FILL(" ",  iIndent)
        "<field name=~"" ipcFieldName "~" class=~"java.lang." ipcDataType "~">" SKIP
        FILL(" ",  iIndent)
        "    <property name=~"net.sf.jasperreports.xpath.field.expression~" value=~"" ipcFieldName "~"/>" SKIP
        FILL(" ",  iIndent)
        "    <fieldDescription><![CDATA[" ipcData "]]></fieldDescription>" SKIP
        FILL(" ",  iIndent)
        "</field>" SKIP
        .

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pJasperGroupDeclarations) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperGroupDeclarations Procedure 
PROCEDURE pJasperGroupDeclarations :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cGroupName AS CHARACTER NO-UNDO.

    DEFINE BUFFER ttColumn FOR ttColumn.
    
    /* groups declarations */
    FOR EACH ttColumn
        WHERE ttColumn.isGroup EQ YES
           BY ttColumn.ttOrder
        :
        ASSIGN
            cGroupName = REPLACE(ttColumn.ttLabel," ","_")
            cGroupName = REPLACE(cGroupName,"[","")
            cGroupName = REPLACE(cGroupName,"]","")
            cFieldName = REPLACE(ttColumn.ttField,"[","")
            cFieldName = REPLACE(cFieldName,"]","")
            .
        PUT STREAM sJasper UNFORMATTED
            "    <group name=~"" REPLACE(cGroupName," ","_") + "_Group~">" SKIP
            "        <groupExpression><![CDATA[$F~{"
            (IF ttColumn.ttTable NE "" THEN ttColumn.ttTable + "__" ELSE "")
            cFieldName "}]]></groupExpression>" SKIP
            .
        IF svShowGroupHeader THEN
        RUN pJasperGroupHeader (ROWID(ttColumn)).
        IF svShowGroupFooter THEN
        RUN pJasperGroupFooter (ROWID(ttColumn)).
        PUT STREAM sJasper UNFORMATTED 
            "    </group>" SKIP
            .
    END. /* each ttColumn */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pJasperGroupFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperGroupFooter Procedure 
PROCEDURE pJasperGroupFooter :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprRowID AS ROWID NO-UNDO.
    
    DEFINE VARIABLE cGroupLabel AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPattern    AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER ttColumn FOR ttColumn.
    DEFINE BUFFER bttColumn FOR ttColumn.    

    FIND FIRST ttColumn WHERE ROWID(ttColumn) EQ iprRowID.
    cGroupLabel = IF ttColumn.ttGroupLabel NE "" THEN ttColumn.ttGroupLabel
                  ELSE "** " + ttColumn.ttLabel + " **"
                  .
    PUT STREAM sJasper UNFORMATTED
        "        <groupFooter>" SKIP
        "            <band height=~"" 20 "~" splitType=~"Stretch~">" SKIP
        "                <staticText>" SKIP
        "                    <reportElement "
        "x=~"" 0 "~" "
        "y=~"" 0 "~" "
        "width=~"" (LENGTH(cGroupLabel) + 6) * {&aoaJasper} "~" "
        "height=~"" 14 "~"/>" SKIP
        "                    <textElement>" SKIP
        "                        <font isBold=~"true~"/>" SKIP
        "                    </textElement>" SKIP
        "                    <text><![CDATA[" cGroupLabel "]]></text>" SKIP
        "                </staticText>" SKIP
        .
    FOR EACH bttColumn
        WHERE bttColumn.isActive    EQ YES
           OR bttColumn.ttGroupCalc NE "",
        EACH ttGroupCalc
        WHERE ttGroupCalc.ttField EQ bttColumn.ttField
          AND ttGroupCalc.ttGroup BEGINS "[Group]"
          AND REPLACE(ttGroupCalc.ttGroup,"[Group] ","") EQ ttColumn.ttLabel
           BY bttColumn.ttOrder
        :
        IF ENTRY(1,ttGroupCalc.ttCalcType,"|") EQ "Calculated" THEN
        cPattern = fJasperCalcPattern(ENTRY(3,ttGroupCalc.ttCalcType,"|")).
        ELSE IF ttGroupCalc.ttCalcType EQ "Count" THEN
        cPattern = "#,###,###,###".
        ELSE
        cPattern = fJasperPattern(bttColumn.ttFormat).
        PUT STREAM sJasper UNFORMATTED
            "                <textField isBlankWhenNull=~"true~" pattern=~"" cPattern "~">" SKIP
            "                    <reportElement "
            "x=~"" IF bttColumn.ttJasperColumn GE 110 THEN bttColumn.ttJasperColumn ELSE 110 "~" "
            "y=~"" 0 "~" "
            "width=~"" bttColumn.ttJasperSize "~" "
            "height=~"" 14 "~"/>" SKIP
            "                    <box>" SKIP
            "                        <topPen lineWidth=~"1.0~"/>" SKIP
            "                    </box>" SKIP
            "                    <textElement textAlignment=~"Right~">" SKIP
            "                        <font isBold=~"true~"/>" SKIP
            "                    </textElement>" SKIP
            "                    <textFieldExpression><![CDATA[$V~{"
            bttColumn.ttField "_" REPLACE(ttColumn.ttLabel," ","_") "_Group"
            "}]]></textFieldExpression>" SKIP
            "                </textField>" SKIP
            .
    END. /* each bttColumn */
    PUT STREAM sJasper UNFORMATTED
        "            </band>" SKIP
        "        </groupFooter>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pJasperGroupHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperGroupHeader Procedure 
PROCEDURE pJasperGroupHeader :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprRowID AS ROWID NO-UNDO.
    
    PUT STREAM sJasper UNFORMATTED
        "        <groupHeader>" SKIP
        "            <band height=~"" 0 "~" splitType=~"Stretch~"/>" SKIP
        "        </groupHeader>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pJasperGroupType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperGroupType Procedure 
PROCEDURE pJasperGroupType :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcGroupType AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER ttColumn FOR ttColumn.
    
    DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPattern   AS CHARACTER NO-UNDO.
    
    FOR EACH ttColumn
        WHERE ttColumn.ttGroupCalc NE "",
        EACH ttGroupCalc
        WHERE ttGroupCalc.ttField EQ ttColumn.ttField
          AND ttGroupCalc.ttGroup EQ ipcGroupType
        BREAK BY ttGroupCalc.ttGroup
              BY ttColumn.ttOrder
        :
        IF FIRST-OF(ttGroupCalc.ttGroup) THEN
        PUT STREAM sJasper UNFORMATTED
            "            <staticText>" SKIP
            "                <reportElement "
            "x=~"" 0 "~" "
            "y=~"" 0 "~" "
            "width=~"" 110 "~" "
            "height=~"" 14 "~"/>" SKIP
            "                <textElement>" SKIP
            "                    <font isBold=~"true~"/>" SKIP
            "                </textElement>" SKIP
            "                <text><![CDATA[** " ttGroupCalc.ttGroup " **]]></text>" SKIP
            "            </staticText>" SKIP
            .
        IF ENTRY(1,ttGroupCalc.ttCalcType,"|") EQ "Calculated" THEN
        cPattern = fJasperCalcPattern(ENTRY(3,ttGroupCalc.ttCalcType,"|")).
        ELSE IF ttGroupCalc.ttCalcType EQ "Count" THEN
        cPattern = "#,###,###,###".
        ELSE
        cPattern = fJasperPattern(ttColumn.ttFormat).
        PUT STREAM sJasper UNFORMATTED
            "            <textField isBlankWhenNull=~"true~" pattern=~"" cPattern "~">" SKIP
            "                <reportElement "
            "x=~"" IF ttColumn.ttJasperColumn GE 110 THEN ttColumn.ttJasperColumn ELSE 110 "~" "
            "y=~"" 0 "~" "
            "width=~"" ttColumn.ttJasperSize "~" "
            "height=~"" 14 "~"/>" SKIP
            "                <box>" SKIP
            "                    <topPen lineWidth=~"1.0~"/>" SKIP
            .
        IF ttGroupCalc.ttGroup EQ "Report" THEN
        PUT STREAM sJasper UNFORMATTED
            "                    <bottomPen lineWidth=~"1.0~"/>" SKIP
            .
        ASSIGN
            cFieldName = ttColumn.ttField
            cFieldName = REPLACE(cFieldName,"[","")
            cFieldName = REPLACE(cFieldName,"]","")
            .
        PUT STREAM sJasper UNFORMATTED
            "                </box>" SKIP
            "                <textElement textAlignment=~"Right~">" SKIP
            "                    <font isBold=~"true~"/>" SKIP
            "                </textElement>" SKIP
            "                <textFieldExpression><![CDATA[$V~{" cFieldName
            "_" ttGroupCalc.ttGroup "Footer}]]></textFieldExpression>" SKIP
            "            </textField>" SKIP
            .
    END. /* each ttColumn */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pJasperJSON) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperJSON Procedure
PROCEDURE pJasperJSON:
/*------------------------------------------------------------------------------
  Purpose:     Export temp-table contents to JSON Format
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hTable       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cColumns     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFullName    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iColumn      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE hQuery       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hQueryBuf    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cDynFunc     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJSONFile    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBufferValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFirstRow    AS LOGICAL   NO-UNDO INITIAL YES.
    
    DEFINE BUFFER ttColumn FOR ttColumn.
    IF VALID-HANDLE(hAppSrv) THEN DO WITH FRAME frameColumns:
        hTable = DYNAMIC-FUNCTION('fGetTableHandle' IN hAppSrv, aoaProgramID).
        IF NOT VALID-HANDLE(hTable) THEN RETURN.
        
        OS-CREATE-DIR "users".
        OS-CREATE-DIR "users\_default".
        OS-CREATE-DIR VALUE("users\" + aoaUserID).
        cJSONFile = "users\" + aoaUserID + "\" + REPLACE(aoaTitle," ","") + ".json".
        OUTPUT STREAM sJasper TO VALUE(cJSONFile).
        PUT STREAM sJasper UNFORMATTED
            "~{" SKIP
            FILL(" ",2)
            "~"" REPLACE(aoaTitle," ","_") "~": ~{" SKIP
            FILL(" ",4)
            "~"" REPLACE(aoaTitle," ","") "~": [" SKIP
            .
        /* run dynamic function (business subject) */
        ASSIGN
            cDynFunc = "f" + REPLACE(aoaTitle," ","")
            hTable = DYNAMIC-FUNCTION(cDynFunc IN hAppSrv, aoaCompany, aoaBatchSeq, aoaUserID)
            .
        IF NOT VALID-HANDLE(hTable) THEN RETURN.

        hTable = hTable:DEFAULT-BUFFER-HANDLE.

        /* scroll returned temp-table records */
        CREATE QUERY hQuery.
        hQuery:SET-BUFFERS(hTable:HANDLE).
        hQuery:QUERY-PREPARE("FOR EACH " + hTable:NAME).
        hQuery:QUERY-OPEN.
        hQueryBuf = hQuery:GET-BUFFER-HANDLE(hTable:NAME).
        REPEAT:
            hQuery:GET-NEXT().
            IF hQuery:QUERY-OFF-END THEN LEAVE.
            IF hQueryBuf:BUFFER-FIELD("RowType"):BUFFER-VALUE() NE "Data" THEN NEXT.
            IF lFirstRow EQ NO THEN
            PUT STREAM sJasper UNFORMATTED "," SKIP.
            ELSE
            lFirstRow = NO.
            PUT STREAM sJasper UNFORMATTED FILL(" ",6) "~{" SKIP.
            FOR EACH ttColumn
                WHERE (ttColumn.isActive    EQ YES
                   OR  ttColumn.isGroup     EQ YES
                   OR  ttColumn.ttGroupCalc NE "")
                  AND  ttColumn.ttFormula   EQ ""
                BREAK BY ttColumn.ttField
                :
                ASSIGN
                    cFullName    = ttColumn.ttField
                    cBufferValue = fFormatValue(hTable, cFullName, ttColumn.ttFormat)
                    cBufferValue = DYNAMIC-FUNCTION("sfWebCharacters", cBufferValue, 8, "Web")
                    cFullName    = ttColumn.ttTable + "__" + cFullName
                    cFullName    = REPLACE(cFullName,"[","")
                    cFullName    = REPLACE(cFullName,"]","")
                    .
                /* handle how jasper auto multiplies % formatted fields by 100 */
                IF INDEX(ttColumn.ttFormat,"%") NE 0 THEN
                ASSIGN
                    cBufferValue = REPLACE(cBufferValue,"%","")
                    cBufferValue = STRING(DECIMAL(cBufferValue) / 100)
                    cBufferValue = cBufferValue + "%"
                    .
                PUT STREAM sJasper UNFORMATTED
                    FILL(" ",8)
                    "~"" cFullName "~": ~""
                    IF cBufferValue EQ ? AND dynValueColumn.dataType EQ "Character" THEN " "
                    ELSE IF cBufferValue EQ ? AND dynValueColumn.dataType NE "Character" THEN "0"
                    ELSE IF cBufferValue NE "" THEN cBufferValue ELSE " "
                    "~""
                    .
                IF NOT LAST(ttColumn.ttField) THEN
                PUT STREAM sJasper UNFORMATTED "," SKIP.
            END. /* each ttColumn */
            PUT STREAM sJasper UNFORMATTED SKIP FILL(" ",6) "}".
        END. /* repeat */
        hQuery:QUERY-CLOSE().
        DELETE OBJECT hQuery.
        PUT STREAM sJasper UNFORMATTED
            SKIP
            FILL(" ",4) "]" SKIP
            FILL(" ",2) "}" SKIP
            "}" SKIP
            .
        OUTPUT STREAM sJasper CLOSE.
        RUN pJasperCopy (cJSONFile).
    END. /* valid happsrv */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pJasperLastPageFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperLastPageFooter Procedure 
PROCEDURE pJasperLastPageFooter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cParameter     AS CHARACTER NO-UNDO EXTENT 100.
    DEFINE VARIABLE cValue         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtDate         AS DATE      NO-UNDO.
    DEFINE VARIABLE idx            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iParameterRow  AS INTEGER   NO-UNDO INITIAL 1.
    
    DEFINE BUFFER bDynValueParam FOR dynValueParam.
    
    CASE ipcType:
        WHEN "user-print" THEN DO:
            IF AVAILABLE user-print THEN
            DO idx = 1 TO EXTENT(user-print.field-name):
                IF user-print.field-name[idx] EQ "svSecure" THEN LEAVE.
                ASSIGN
                    cParameter[iParameterRow] = IF INDEX(user-print.field-name[idx],"Sort") NE 0 THEN "Sort By"
                        ELSE IF user-print.field-label[idx] EQ ? THEN REPLACE(user-print.field-name[idx],"sv","")
                        ELSE user-print.field-label[idx]
                    cParameter[iParameterRow] = cParameter[iParameterRow] + ": @@@"
                    cValue = IF user-print.field-value[idx] NE ? THEN user-print.field-value[idx] ELSE ""
                    .
                IF user-print.field-name[idx] BEGINS "svAll" THEN
                ASSIGN
                    cParameter[iParameterRow] = cParameter[iParameterRow] + ", "
                                              + user-print.field-label[idx + 1] + ": "
                                              + user-print.field-value[idx + 1] + "; "
                                              + user-print.field-label[idx + 2] + ": "
                                              + user-print.field-value[idx + 2]
                    idx = idx + 2
                    .
                ELSE IF user-print.field-label[idx + 1] EQ ? AND
                    INDEX(user-print.field-name[idx + 1],"DateOption") NE 0 THEN DO:
                    dtDate = DATE(user-print.field-value[idx]) NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN
                    dtDate = ?.
                    ASSIGN
                        cParameter[iParameterRow] = cParameter[iParameterRow] + " (" + user-print.field-value[idx + 1] + ")"
                        cValue = STRING(DYNAMIC-FUNCTION("fDateOptionDate" IN hAppSrvBin, user-print.field-value[idx + 1], dtDate),"99/99/9999")
                        idx = idx + 1
                        .
                END.
                ELSE IF INDEX(user-print.field-name[idx + 1],"AMPM") NE 0 THEN
                ASSIGN
                    cParameter[iParameterRow] = cParameter[iParameterRow] + " " + user-print.field-value[idx + 1]
                    idx = idx + 1
                    .
                ASSIGN
                    cParameter[iParameterRow] = REPLACE(cParameter[iParameterRow],"@@@",cValue)
                    iParameterRow = iParameterRow + 1
                    .
            END. /* do idx */
        END. /* user-print */
        WHEN "dynParamValue" THEN DO:
            IF AVAILABLE dynParamValue THEN
            FOR EACH dynValueParam NO-LOCK
                WHERE dynValueParam.subjectID    EQ dynParamValue.subjectID
                  AND dynValueParam.user-id      EQ dynParamValue.user-id
                  AND dynValueParam.prgmName     EQ dynParamValue.prgmName
                  AND dynValueParam.paramValueID EQ dynParamValue.paramValueID
                   BY dynValueParam.sortOrder
                :
                IF dynValueParam.paramName BEGINS "svS" THEN NEXT.
                ASSIGN
                    cParameter[iParameterRow] = IF dynValueParam.paramLabel EQ ? THEN REPLACE(dynValueParam.paramName,"sv","")
                                                ELSE REPLACE(dynValueParam.paramLabel,":","")
                    cParameter[iParameterRow] = cParameter[iParameterRow] + ": @@@"
                    cValue = IF dynValueParam.paramValue NE ? AND
                                dynValueParam.paramValue NE CHR(254) THEN dynValueParam.paramValue ELSE ""
                    .
                FIND FIRST bDynValueParam NO-LOCK
                     WHERE bDynValueParam.subjectID    EQ dynValueParam.subjectID
                       AND bDynValueParam.user-id      EQ dynValueParam.user-id
                       AND bDynValueParam.prgmName     EQ dynValueParam.prgmName
                       AND bDynValueParam.paramValueID EQ dynValueParam.paramValueID
                       AND bDynValueParam.sortOrder    EQ dynValueParam.sortOrder + 1
                     NO-ERROR.
                IF AVAILABLE bDynValueParam AND
                   bDynValueParam.paramLabel EQ ? AND
                   INDEX(bDynValueParam.paramName,"DatePickList") NE 0 THEN DO:
                    dtDate = DATE(dynValueParam.paramValue) NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN
                    dtDate = ?.
                    ASSIGN
                        cParameter[iParameterRow] = cParameter[iParameterRow] + " (" + bDynValueParam.paramValue + ")"
                        cValue = STRING(DYNAMIC-FUNCTION("fDateOptionDate" IN hAppSrvBin, bDynValueParam.paramValue, dtDate),"99/99/9999")
                        .
                END. /* if avail */
                IF cValue EQ ? THEN
                cValue = "".
                ASSIGN
                    cParameter[iParameterRow] = REPLACE(cParameter[iParameterRow],"@@@",cValue)
                    iParameterRow = iParameterRow + 1
                    .
            END. /* each dynvalueparam */
        END. /* dynparamvalue */
    END CASE.
    
    IF dynParamValue.pageHeight GE (iParameterRow + 3) * 14 THEN DO:
        /* last page footer band */
        PUT STREAM sJasper UNFORMATTED
            "    <lastPageFooter>" SKIP
            "        <band height=~"" (iParameterRow + 3) * 14 "~" splitType=~"Stretch~">" SKIP
            .
        IF svShowPageFooter THEN
        RUN pJasperGroupType ("Page").
        PUT STREAM sJasper UNFORMATTED
            "            <rectangle>" SKIP
            "                <reportElement mode=~"Transparent~" "
            "x=~"" 0 "~" "
            "y=~"" 14 "~" "
            "width=~"" 560 "~" "
            "height=~"" (iParameterRow - 1) * 14 "~"/>" SKIP
            "            </rectangle>" SKIP
            "            <staticText>" SKIP
            "                <reportElement "
            "x=~"" 0 "~" "
            "y=~"" 14 "~" "
            "width=~"" 56 "~" "
            "height=~"" 14 "~"/>" SKIP
            "                <textElement>" SKIP
            "                    <font isBold=~"true~" isUnderline=~"true~"/>" SKIP
            "                </textElement>" SKIP
            "                <text><![CDATA[Parameters:]]></text>" SKIP
            "            </staticText>" SKIP
            .
        DO idx = 1 TO iParameterRow:
            IF cParameter[idx] NE "" AND cParameter[idx] NE CHR(254) THEN
            PUT STREAM sJasper UNFORMATTED
                "            <staticText>" SKIP
                "                <reportElement "
                "x=~"" 60 "~" "
                "y=~"" (idx) * 14 "~" "
                "width=~"" 500 "~" "
                "height=~"" 14 "~"/>" SKIP
                "                <text><![CDATA[" cParameter[idx] "]]></text>" SKIP
                "            </staticText>" SKIP
                .
        END. /* do idx */
        RUN pJasperPageBottom (iParameterRow * 14).
        PUT STREAM sJasper UNFORMATTED
            "        </band>" SKIP
            "    </lastPageFooter>" SKIP
            .
    END. /* if last page footer fits page height */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pJasperPageBottom) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperPageBottom Procedure 
PROCEDURE pJasperPageBottom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiRow AS INTEGER NO-UNDO.

    IF CAN-FIND(FIRST ttGroupCalc
                WHERE ttGroupCalc.ttGroup EQ "Page") THEN
    ipiRow = ipiRow + 14.
    PUT STREAM sJasper UNFORMATTED
        "            <textField pattern=~"MMM d, yyyy h:mm:ss a~">" SKIP
        "                <reportElement "
        "x=~"" 0 "~" "
        "y=~"" ipiRow "~" "
        "width=~"" 180 "~" "
        "height=~"" 14 "~"/>" SKIP
        "                <textFieldExpression><![CDATA[new java.util.Date()]]></textFieldExpression>" SKIP
        "            </textField>" SKIP
        "            <staticText>" SKIP
        "                <reportElement "
        "x=~"" 0 "~" "
        "y=~"" ipiRow + 14 "~" "
        "width=~"" 26 "~" "
        "height=~"" 14 "~"/>" SKIP
        "                <text><![CDATA[Page:]]></text>" SKIP
        "            </staticText>" SKIP
        "            <textField>" SKIP
        "                <reportElement "
        "x=~"" 30 "~" "
        "y=~"" ipiRow + 14 "~" "
        "width=~"" 100 "~" "
        "height=~"" 14 "~"/>" SKIP
        "                <textFieldExpression><![CDATA[$V~{PAGE_NUMBER}]]></textFieldExpression>" SKIP
        "            </textField>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pJasperPageFooterBand) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperPageFooterBand Procedure 
PROCEDURE pJasperPageFooterBand :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* page footer band */
    PUT STREAM sJasper UNFORMATTED
        "    <pageFooter>" SKIP
        "        <band height=~"" 44 "~" splitType=~"Stretch~">" SKIP
        .
    RUN pJasperGroupType ("Page").    
    RUN pJasperPageBottom (0).
    PUT STREAM sJasper UNFORMATTED
        "        </band>" SKIP
        "    </pageFooter>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pJasperPageHeaderBand) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperPageHeaderBand Procedure 
PROCEDURE pJasperPageHeaderBand :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* page header band */
    PUT STREAM sJasper UNFORMATTED
        "    <pageHeader>" SKIP
        "        <band height=~"" 0 "~" splitType=~"Stretch~"/>" SKIP
        "    </pageHeader>" SKIP
        .    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pJasperQueryString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperQueryString Procedure 
PROCEDURE pJasperQueryString :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcQuery1 AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcQuery2 AS CHARACTER NO-UNDO.

    PUT STREAM sJasper UNFORMATTED
        FILL(" ", iIndent)
        "<queryString language=~"json~">" SKIP
        FILL(" ", iIndent)
        "    <![CDATA[" ipcQuery1 "." ipcQuery2 "]]>" SKIP
        FILL(" ", iIndent)
        "</queryString>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pJasperReport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperReport Procedure 
PROCEDURE pJasperReport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcReport AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcType   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiSize   AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE iMargin AS INTEGER NO-UNDO.
    
    iMargin = IF CAN-DO("pdf,view,docx",ipcType) THEN 20 ELSE 0.

    CASE ipcReport:
        WHEN "Open" THEN DO:
            PUT STREAM sJasper UNFORMATTED
                "<?xml version=~"1.0~" encoding=~"UTF-8~"?>" SKIP
                "<!-- Created with Jaspersoft Studio version 6.6.0.final using JasperReports Library version 6.6.0  -->" SKIP
                "<jasperReport xmlns=~"http://jasperreports.sourceforge.net/jasperreports~" "
                "xmlns:xsi=~"http://www.w3.org/2001/XMLSchema-instance~" "
                "xsi:schemaLocation=~"http://jasperreports.sourceforge.net/jasperreports "
                "http://jasperreports.sourceforge.net/xsd/jasperreport.xsd~" "
                "name=~"" REPLACE(aoaTitle," ","") "~" "
                .
            IF AVAILABLE dynParamValue AND dynParamValue.pageHeight GT 0 THEN
            PUT STREAM sJasper UNFORMATTED
                "pageHeight=~"" dynParamValue.pageHeight "~" "
                . 
            PUT STREAM sJasper UNFORMATTED
                "pageWidth=~"" ipiSize "~" "
                "columnWidth=~"" ipiSize - iMargin * 2 "~" "
                "orientation=~"" IF AVAILABLE dynParamValue THEN dynParamValue.pageOrientation ELSE "Landscape" ""~" "
                "leftMargin=~"" iMargin "~" "
                "rightMargin=~"" iMargin "~" "
                "topMargin=~"" iMargin "~" "
                "bottomMargin=~"" iMargin "~" "
                "isIgnorePagination=~"" TRIM(STRING(CAN-DO("csv,xls",ipcType),"true/false")) "~""
                ">" SKIP.
        END. /* open */
        WHEN "Close" THEN
        PUT STREAM sJasper UNFORMATTED
            "</jasperReport>" SKIP
            .
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pJasperStarter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperStarter Procedure 
PROCEDURE pJasperStarter :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcType       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTaskRecKey AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJasperFile AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cFileName      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJasperFile    AS CHARACTER NO-UNDO EXTENT 5.
    DEFINE VARIABLE cJasperStarter AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTemplate      AS CHARACTER NO-UNDO EXTENT 2.
    DEFINE VARIABLE cText          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserFolder    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtDate         AS DATE      NO-UNDO.
    DEFINE VARIABLE idx            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTime          AS INTEGER   NO-UNDO.
    
    RUN pCreateDir (OUTPUT cUserFolder).
    OUTPUT TO VALUE(cUserFolder + "JasperStarter.log").
    OUTPUT CLOSE.
    FOR EACH ttTaskFile:
        ASSIGN
            idx          = idx + 1
            cFileName    = REPLACE(aoaTitle," ","") + "." + ipcTaskRecKey
            cTemplate[1] = SEARCH(cUserFolder + cFileName + ".jrxml")
            cTemplate[2] = cTemplate[1]
            .
        IF dynParamValue.onePer AND cTemplate[1] NE ? THEN DO:
            cTemplate[2] = REPLACE(cTemplate[1],".jrxml",STRING(idx) + ".jrxml").
            INPUT STREAM sTemplate FROM VALUE(cTemplate[1]) NO-ECHO.
            OUTPUT STREAM sJasper TO VALUE(cTemplate[2]).
            REPEAT:
                IMPORT STREAM sTemplate UNFORMATTED cText.
                IF INDEX(cText,"net.sf.jasperreports.data.adapter") NE 0 THEN NEXT.

                IF INDEX(cText,"com.jaspersoft.studio.data.defaultdataadapter") NE 0 THEN
                cText = '    <property name="com.jaspersoft.studio.data.defaultdataadapter" value="One Empty Record"/>'.

                IF INDEX(cText,"jsonFormFile") NE 0 THEN
                cText = REPLACE(cText,"jsonFormFile",ttTaskFile.taskFile).

                IF cText NE "" THEN
                PUT STREAM sJasper UNFORMATTED cText SKIP.
                ELSE
                PUT STREAM sJasper UNFORMATTED SKIP(1).
            END. /* repeat */
            OUTPUT STREAM sJasper CLOSE.
            INPUT STREAM sTemplate CLOSE.
        END. /* if oneper */
        ASSIGN
            dtDate         = TODAY
            iTime          = TIME
            cJasperFile[1] = SEARCH(cTemplate[2])
            cJasperFile[2] = ttTaskFile.taskFile //SEARCH(cUserFolder + cFileName + ".json")
            cJasperFile[3] = REPLACE(cJasperFile[1],"jrxml",ipcType)
            cJasperFile[3] = REPLACE(cJasperFile[3]," -d","")
            cJasperFile[4] = "TaskResults/"
                           + REPLACE(aoaTitle," ","") + "."
                           + STRING(YEAR(dtDate),"9999")
                           + STRING(MONTH(dtDate),"99")
                           + STRING(DAY(dtDate),"99") + "."
                           + STRING(iTime,"99999")
            cJasperFile[5] = IF ipcType EQ "view" THEN REPLACE(cJasperFile[2],".json",".err")
                             ELSE cJasperFile[4] + ".err"
            opcJasperFile  = opcJasperFile + cJasperFile[4] + "." + LC(ipcType) + ","
            jdx            = jdx + 1
            cJasperStarter = "jasperstarter process "
                           + "-f " + LC(ipcType) + " "
                           + "-t json "
                           + "-o " + cJasperFile[4] + " "
                           + "--data-file "
                           + cJasperFile[2] + " "
                           + "--json-query "
                           + REPLACE(aoaTitle," ","_")
                           + "." + REPLACE(aoaTitle," ","") + " "
                           +  cJasperFile[1] + " "
/*                           + (IF dynParamValue.onePer THEN "-P jsonData=~"" + cJasperFile[2] + "~" " ELSE "")*/
                           + "1>NUL 2>"
                           + cJasperFile[5]
                           .
        DO idx = 1 TO EXTENT(cJasperFile) - 1:
            IF cJasperFile[idx] EQ ? THEN DO:
                MESSAGE 
                    "Unable to run" aoaTitle "Jasper Report" SKIP 
                    "Jasper Files .jrxml and/or .json not found!"
                VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END. /* if ? */
        END. /* do idx */
        
        IF NOT CAN-DO("print -d,view",ipcType) THEN DO TRANSACTION:
            CREATE TaskResult.
            ASSIGN
                TaskResult.fileDateTime = DATETIME(dtDate,iTime * 1000)
                TaskResult.fileType     = ipcType
                TaskResult.user-id      = aoaUserID
                TaskResult.folderFile   = ENTRY(jdx,opcJasperFile)
                .
        END. /* if not can-do */
        /* log jasperstarter command for debug purposes if needed */
        OUTPUT TO VALUE(cUserFolder + "JasperStarter.log") APPEND.
        PUT UNFORMATTED cJasperStarter SKIP.
        PUT UNFORMATTED
            REPLACE(
            REPLACE(cJasperStarter," -o " + cJasperFile[4],""),
            REPLACE(cUserFolder,"/","~\"),"")
            SKIP(1)
            .
        OUTPUT CLOSE.
        OS-DELETE VALUE(cJasperFile[3]).
        IF ipcType EQ "view" THEN
        OS-COMMAND NO-WAIT START VALUE(cJasperStarter).
        ELSE
        OS-COMMAND SILENT CALL VALUE(cJasperStarter).
    END. /* each tttaskfile */
    opcJasperFile = TRIM(opcJasperFile,",").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pJasperStyles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperStyles Procedure 
PROCEDURE pJasperStyles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cColor AS CHARACTER NO-UNDO.
    
    cColor = "#DCF5EB".
    PUT STREAM sJasper UNFORMATTED
        "    <style name=~"Zebra~" mode=~"Transparent~">" SKIP
        "        <conditionalStyle>" SKIP
        "            <conditionExpression><![CDATA[$V~{REPORT_COUNT}%2 == 1]]></conditionExpression>" SKIP
        "            <style backcolor=~"" cColor "~"/>" SKIP
        "        </conditionalStyle>" SKIP
        "    </style>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pJasperSubDataSet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperSubDataSet Procedure
PROCEDURE pJasperSubDataSet:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcBand AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cDataType  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHandle    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLabel     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParam     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPattern   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTables    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTitle     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hTable     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iField     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iFieldIdx  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iRow       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTables    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iWidth     AS INTEGER   NO-UNDO.

    RUN spGetSessionParam (ipcType + "Tables", OUTPUT cTables).
    iTables = INTEGER(cTables).
    IF iTables NE 0 THEN
    DO idx = 1 TO iTables:
        CASE ipcType:
            WHEN "Detail" THEN
            IF NOT lDetailDataSet[idx] THEN NEXT.
            WHEN "Summary" THEN
            IF NOT lSummaryDataSet[idx] THEN NEXT.
        END CASE.
        iRow = iRow + 1.
        RUN spGetSessionParam (ipcType + "Handle" + STRING(idx), OUTPUT cHandle).
        hTable = WIDGET-HANDLE(cHandle).
        IF NOT VALID-HANDLE(hTable) THEN NEXT.
        RUN spGetSessionParam (ipcType + "Title" + STRING(idx), OUTPUT cTitle).
        ASSIGN
            hTable = hTable:DEFAULT-BUFFER-HANDLE
            iField = 0
            .
        CASE ipcBand:
            WHEN "FieldDeclarations" THEN DO:
                PUT STREAM sJasper UNFORMATTED
                    "    <subDataset name=~"" ipcType STRING(idx) "~">" SKIP
                    "        <property name=~"net.sf.jasperreports.json.source~" value=~"" REPLACE(cJasperFile,"jrxml","json") "~"/>" SKIP
                    .
                IF ipcType EQ "Detail" THEN DO:
                    PUT STREAM sJasper UNFORMATTED
                        FILL(" ", iIndent)
                        "<parameter name=~"paramRecordID~" class=~"java.lang.String~"/>" SKIP
                        .
                    cParam = "(" + hTable:NAME + "__recordID == $P~{paramRecordID})".
                END. /* if detail */
                RUN pJasperQueryString (REPLACE(aoaTitle," ","_"), ipcType + STRING(idx) + cParam).
            END.
            WHEN "ComponentElement" THEN DO:
                DO iFieldIdx = 1 TO hTable:NUM-FIELDS:
                    IF CAN-DO("rowType,parameters,recDataType,recordID",hTable:BUFFER-FIELD(iFieldIdx):NAME) THEN NEXT.
                    IF hTable:BUFFER-FIELD(iFieldIdx):NAME BEGINS "xx" THEN NEXT.
                    iWidth = iWidth
                           + MAX(hTable:BUFFER-FIELD(iFieldIdx):WIDTH,LENGTH(hTable:BUFFER-FIELD(iFieldIdx):LABEL))
                           * {&aoaJasper}
                           .
                END. /* do ifieldidx */
                PUT STREAM sJasper UNFORMATTED
                    "            <componentElement>" SKIP
                    .
                IF ipcType EQ "Summary" THEN
                PUT STREAM sJasper UNFORMATTED
                    "                <reportElement x=~"43~" y=~"14~" width=~"" iWidth "~" height=~"72~" isRemoveLineWhenBlank=~"true~">" SKIP
                    .
                ELSE
                PUT STREAM sJasper UNFORMATTED
                    "                <reportElement key=~"~" style=~"" ipcType STRING(idx) "_CH~" isPrintRepeatedValues=~"false~" x=~"43~" y=~"" (iRow - 1) * 42 + 14 "~" width=~"" iWidth "~" height=~"42~" isRemoveLineWhenBlank=~"true~">" SKIP
                    .
                PUT STREAM sJasper UNFORMATTED
                    "                    <property name=~"com.jaspersoft.studio.layout~" value=~"com.jaspersoft.studio.editor.layout.VerticalRowLayout~"/>" SKIP
                    .
                IF ipcType EQ "Summary" THEN
                PUT STREAM sJasper UNFORMATTED
                    "                    <property name=~"com.jaspersoft.studio.table.style.table_header~" value=~"" ipcType STRING(idx) "_TH~"/>" SKIP
                    .
                PUT STREAM sJasper UNFORMATTED
                    "                    <property name=~"com.jaspersoft.studio.table.style.column_header~" value=~"" ipcType STRING(idx) "_CH~"/>" SKIP
                    "                    <property name=~"com.jaspersoft.studio.table.style.detail~" value=~"" ipcType STRING(idx) "_TD~"/>" SKIP
                    .
                IF ipcType EQ "Detail" THEN
                PUT STREAM sJasper UNFORMATTED
                    "                    <property name=~"net.sf.jasperreports.export.headertoolbar.table.name~" value=~"~"/>" SKIP
                    .
                PUT STREAM sJasper UNFORMATTED
                    "                </reportElement>" SKIP
                    "                <jr:table xmlns:jr=~"http://jasperreports.sourceforge.net/jasperreports/components~" xsi:schemaLocation=~"http://jasperreports.sourceforge.net/jasperreports/components http://jasperreports.sourceforge.net/xsd/components.xsd~">" SKIP
                    "                    <datasetRun subDataset=~"" ipcType STRING(idx) "~">" SKIP
                    .
                IF ipcType EQ "Summary" THEN
                PUT STREAM sJasper UNFORMATTED
                    "                        <connectionExpression><![CDATA[$P~{REPORT_CONNECTION}]]></connectionExpression>" SKIP
                    .
                ELSE DO:
                    FIND FIRST ttColumn
                         WHERE ttColumn.ttField EQ "recordID"
                         NO-ERROR.
                    IF AVAILABLE ttColumn THEN
                    PUT STREAM sJasper UNFORMATTED
                        "                        <datasetParameter name=~"paramRecordID~">" SKIP
                        "                            <datasetParameterExpression><![CDATA[$F~{" ttColumn.ttTable "__recordID}]]></datasetParameterExpression>" SKIP
                        "                        </datasetParameter>" SKIP
                        .
                END. /* else */
                PUT STREAM sJasper UNFORMATTED
                    "                    </datasetRun>" SKIP
                    .
                IF ipcType EQ "Summary" THEN
                PUT STREAM sJasper UNFORMATTED
                    "                    <jr:columnGroup width=~"" iWidth "~">" SKIP
                    "                        <property name=~"com.jaspersoft.studio.components.table.model.column.name~" value=~"Columns [2]~"/>" SKIP
                    "                        <jr:tableHeader style=~"" ipcType STRING(idx) "_TH~" height=~"30~" rowSpan=~"1~">" SKIP
                    "                            <staticText>" SKIP
                    "                                <reportElement x=~"0~" y=~"0~" width=~"" iWidth "~" height=~"30~"/>" SKIP
                    "                                <textElement textAlignment=~"Center~" verticalAlignment=~"Middle~">" SKIP
                    "                                    <font isBold=~"true~"/>" SKIP
                    "                                </textElement>" SKIP
                    "                                <text><![CDATA[" cTitle "]]></text>" SKIP
                    "                            </staticText>" SKIP
                    "                        </jr:tableHeader>" SKIP
                    .
            END.
        END CASE.
        DO iFieldIdx = 1 TO hTable:NUM-FIELDS:
            IF CAN-DO("rowType,parameters,recDataType,recordID",hTable:BUFFER-FIELD(iFieldIdx):NAME) THEN NEXT.
            IF hTable:BUFFER-FIELD(iFieldIdx):NAME BEGINS "xx" THEN NEXT.
            ASSIGN
                cFieldName = hTable:BUFFER-FIELD(iFieldIdx):NAME
                cFieldName = hTable:NAME + "__" + cFieldName
                cDataType  = fDataType(hTable:BUFFER-FIELD(iFieldIdx):DATA-TYPE)
                cLabel     = hTable:BUFFER-FIELD(iFieldIdx):LABEL
                .
            CASE ipcBand:
                WHEN "FieldDeclarations" THEN DO:
                    RUN pJasperFieldDeclarationsPut (cFieldName, cDataType, cFieldName).
                END.
                WHEN "ComponentElement" THEN DO:
                    ASSIGN
                        cPattern = ""
                        iWidth   = MAX(hTable:BUFFER-FIELD(iFieldIdx):WIDTH,LENGTH(hTable:BUFFER-FIELD(iFieldIdx):LABEL))
                                 * {&aoaJasper}
                                 .
                    IF CAN-DO("Integer,Decimal",hTable:BUFFER-FIELD(iFieldIdx):DATA-TYPE) THEN
                    cPattern = " pattern=~"" + fJasperPattern(hTable:BUFFER-FIELD(iFieldIdx):FORMAT) + "~"".
                    PUT STREAM sJasper UNFORMATTED
                        "                        <jr:column width=~"" iWidth "~">" SKIP
                        "                            <property name=~"com.jaspersoft.studio.components.table.model.column.name~" value=~"" cFieldName "~"/>" SKIP
                        .
                    IF ipcType EQ "Summary" THEN
                    PUT STREAM sJasper UNFORMATTED
                        "                            <jr:tableHeader style=~"" ipcType STRING(idx) "_TH~" height=~"0~" rowSpan=~"1~"/>" SKIP
                        "                            <jr:tableFooter style=~"" ipcType STRING(idx) "_TH~" height=~"7~" rowSpan=~"1~"/>" SKIP
                        .
                    PUT STREAM sJasper UNFORMATTED
                        "                            <jr:columnHeader style=~"" ipcType STRING(idx) "_CH~" height=~"14~" rowSpan=~"1~">" SKIP
                        "                                <staticText>" SKIP
                        "                                    <reportElement x=~"0~" y=~"0~" width=~"" iWidth "~" height=~"14~"/>" SKIP
                        "                                    <textElement>" SKIP
                        "                                        <font isBold=~"true~" isUnderline=~"true~"/>" SKIP
                        "                                    </textElement>" SKIP
                        "                                    <text><![CDATA[" cLabel "]]></text>" SKIP
                        "                                </staticText>" SKIP
                        "                            </jr:columnHeader>" SKIP
                        "                            <jr:columnFooter style=~"" ipcType STRING(idx) "_CH~" height=~""
                        IF ipcType EQ "Summary" THEN 0 ELSE 7 "~" rowSpan=~"1~"/>" SKIP
                        "                            <jr:detailCell style=~"" ipcType STRING(idx) "_TD~" height=~"14~">" SKIP
                        "                                <textField isBlankWhenNull=~"true~"" cPattern ">" SKIP
                        "                                    <reportElement x=~"0~" y=~"0~" width=~"" iWidth "~" height=~"14~">" SKIP
                        "                                        <property name=~"com.jaspersoft.studio.spreadsheet.connectionID~"/>" SKIP
                        "                                    </reportElement>" SKIP
                        "                                    <textFieldExpression><![CDATA[$F~{" cFieldName "}]]></textFieldExpression>" SKIP
                        "                                </textField>" SKIP
                        "                            </jr:detailCell>" SKIP
                        "                        </jr:column>" SKIP
                        .
                END.
            END CASE.
        END. /* do ifieldidx */
        CASE ipcBand:
            WHEN "FieldDeclarations" THEN DO:
                PUT STREAM sJasper UNFORMATTED
                    "    </subDataset>" SKIP
                    .
            END.
            WHEN "ComponentElement" THEN DO:
                IF ipcType EQ "Summary" THEN
                PUT STREAM sJasper UNFORMATTED
                    "                    </jr:columnGroup>" SKIP
                    .
                PUT STREAM sJasper UNFORMATTED
                    "                </jr:table>" SKIP
                    "            </componentElement>" SKIP
                    .
            END.
        END CASE.
    END. /* do idx */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pJasperSummaryBand) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperSummaryBand Procedure 
PROCEDURE pJasperSummaryBand :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* summary band */
    PUT STREAM sJasper UNFORMATTED
        "    <summary>" SKIP
        "        <band height=~"" 86 "~" splitType=~"Stretch~">" SKIP
        .
    RUN pJasperGroupType ("Report").
    RUN pJasperSubDataSet ("Summary", "ComponentElement").
    PUT STREAM sJasper UNFORMATTED
        "        </band>" SKIP
        "    </summary>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pJasperVariableDeclarations) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperVariableDeclarations Procedure 
PROCEDURE pJasperVariableDeclarations :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDataType   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cName       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResetGroup AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER ttColumn FOR ttColumn.

    /* variable declarations */
    FOR EACH ttColumn
        WHERE ttColumn.isGroup     EQ YES
           OR ttColumn.ttGroupCalc NE "",
        EACH ttGroupCalc
        WHERE ttGroupCalc.ttField EQ ttColumn.ttField
           BY ttColumn.ttOrder
        :
        IF NOT svShowGroupFooter  AND ttGroupCalc.ttGroup BEGINS "[Group] " THEN NEXT.
        IF NOT svShowGroupFooter  AND ttGroupCalc.ttGroup EQ "Column"       THEN NEXT.
        IF NOT svShowPageFooter   AND ttGroupCalc.ttGroup EQ "Page"         THEN NEXT.
        IF NOT svShowReportFooter AND ttGroupCalc.ttGroup EQ "Report"       THEN NEXT.
        CASE ttColumn.ttType:
            WHEN "Character" THEN
            cDataType = "String".
            WHEN "Decimal" THEN
            cDataType = "Double".
            WHEN "Integer" THEN
            cDataType = "Integer".
            OTHERWISE
            cDataType = "String".
        END CASE.
        cDataType = IF ENTRY(1,ttGroupCalc.ttGroup,"|") NE "Calculated" THEN cDataType
                    ELSE ENTRY(3,ttGroupCalc.ttGroup,"|").
        IF ttGroupCalc.ttCalcType EQ "count" THEN
        cDataType = "Integer".
        ASSIGN
            cResetGroup = REPLACE(REPLACE(ttGroupCalc.ttGroup,"[Group] ","")," ","_") + "_Group"
            cName       = ttGroupCalc.ttField + "_"
                        + IF ttGroupCalc.ttGroup BEGINS "[Group] " THEN cResetGroup
                          ELSE ttGroupCalc.ttGroup + "Footer"
                        .
        PUT STREAM sJasper UNFORMATTED
            "    <variable name=~"" cName "~" class=~"java.lang." cDataType
            .
        IF ttGroupCalc.ttGroup BEGINS "[Group] " THEN
        PUT STREAM sJasper UNFORMATTED
            "~" resetType=~"Group~" resetGroup=~"" cResetGroup
            .
        ELSE IF ttGroupCalc.ttGroup NE "Report" THEN
        PUT STREAM sJasper UNFORMATTED
            "~" resetType=~"" ttGroupCalc.ttGroup
            .
        IF ENTRY(1,ttGroupCalc.ttCalcType,"|") NE "Calculated" THEN
        PUT STREAM sJasper UNFORMATTED
            "~" calculation=~"" ttGroupCalc.ttCalcType
            .
        PUT STREAM sJasper UNFORMATTED 
             "~">" SKIP
            "        <variableExpression><![CDATA["
            .
        IF ENTRY(1,ttGroupCalc.ttCalcType,"|") EQ "Calculated" THEN
        PUT STREAM sJasper UNFORMATTED
            ENTRY(2,ttGroupCalc.ttCalcType,"|")
            .
        ELSE
        IF ttColumn.ttFormula NE "" THEN
        PUT STREAM sJasper UNFORMATTED
            ttColumn.ttField
            .
        ELSE
        PUT STREAM sJasper UNFORMATTED
            "$F~{"
            (IF ttColumn.ttTable NE "" THEN ttColumn.ttTable + "__" ELSE "")
            ttColumn.ttField "}"
            .
        PUT STREAM sJasper UNFORMATTED
            "]]></variableExpression>" SKIP
            "    </variable>" SKIP
            .
    END. /* each ttColumn */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-pJasterTitleBand) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasterTitleBand Procedure 
PROCEDURE pJasterTitleBand :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiSize AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE cSubTitle   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParamValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtDate      AS DATE      NO-UNDO.
    DEFINE VARIABLE idx         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lParamFound AS LOGICAL   NO-UNDO.

    DEFINE BUFFER bDynValueParam FOR dynValueParam.

    /* title band */
    FOR EACH dynSubjectParamSet NO-LOCK
        WHERE dynSubjectParamSet.subjectID  EQ dynParamValue.subjectID
          AND dynSubjectParamSet.isVisible  EQ YES
          AND dynSubjectParamSet.useInTitle EQ YES,
        EACH dynParamSetDtl NO-LOCK
        WHERE dynParamSetDtl.paramSetID  EQ dynSubjectParamSet.paramSetID
          AND dynParamSetDtl.paramPrompt EQ YES
           BY dynSubjectParamSet.sortOrder
        :
        ASSIGN
            cParamValue = ""
            lParamFound = NO
            .
        FIND FIRST dynValueParam NO-LOCK
             WHERE dynValueParam.subjectID    EQ dynParamValue.subjectID
               AND dynValueParam.user-id      EQ dynParamValue.user-id
               AND dynValueParam.prgmName     EQ dynParamValue.prgmName
               AND dynValueParam.paramValueID EQ dynParamValue.paramValueID
               AND dynValueParam.paramName    EQ dynParamSetDtl.paramName
             NO-ERROR.
        IF AVAILABLE dynValueParam THEN DO:
            ASSIGN
                cParamValue = dynValueParam.paramValue
                lParamFound = YES
                .
            FIND FIRST bDynValueParam NO-LOCK
                 WHERE bDynValueParam.subjectID    EQ dynValueParam.subjectID
                   AND bDynValueParam.user-id      EQ dynValueParam.user-id
                   AND bDynValueParam.prgmName     EQ dynValueParam.prgmName
                   AND bDynValueParam.paramValueID EQ dynValueParam.paramValueID
                   AND bDynValueParam.sortOrder    EQ dynValueParam.sortOrder + 1
                 NO-ERROR.
            IF AVAILABLE bDynValueParam AND
               bDynValueParam.paramLabel EQ ? AND
               INDEX(bDynValueParam.paramName,"DatePickList") NE 0 THEN DO:
                dtDate = DATE(dynValueParam.paramValue) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                dtDate = ?.
                cParamValue = STRING(DYNAMIC-FUNCTION("fDateOptionDate" IN hAppSrvBin, bDynValueParam.paramValue, dtDate),"99/99/9999").
            END. /* if avail */
        END. /* if avail */
        IF cParamValue EQ CHR(254) THEN
        cParamValue = "".
        IF lParamFound THEN
        cSubTitle = cSubTitle + dynParamSetDtl.paramLabel + ": "
                  + cParamValue
                  + " - "
                  .
    END. /* each dynsubjectparamset */
    cSubTitle = TRIM(cSubTitle," - ").
    PUT STREAM sJasper UNFORMATTED
        "    <title>" SKIP
        "        <band height=~"" IF cSubTitle EQ "" THEN 40 ELSE 80 "~" splitType=~"Stretch~">" SKIP
        "            <staticText>" SKIP
        "                <reportElement x=~"" 0 "~" y=~"" 0 "~" width=~"" ipiSize "~" height=~"" 40 "~"/>" SKIP
        "                <textElement>" SKIP
        "                    <font size=~"" 26 "~"/>" SKIP
        "                </textElement>" SKIP
        "                <text><![CDATA[" aoaTitle "]]></text>" SKIP
        "            </staticText>" SKIP
        .
    IF cSubTitle NE "" THEN
    PUT STREAM sJasper UNFORMATTED
        "            <staticText>" SKIP
        "                <reportElement x=~"" 0 "~" y=~"" 40 "~" width=~"" ipiSize "~" height=~"" 40 "~"/>" SKIP
        "                <textElement>" SKIP
        "                    <font size=~"" 9 "~"/>" SKIP
        "                </textElement>" SKIP
        "                <text><![CDATA[" cSubTitle "]]></text>" SKIP
        "            </staticText>" SKIP
        .
    PUT STREAM sJasper UNFORMATTED
        "        </band>" SKIP
        "    </title>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pLocalCSV) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLocalCSV Procedure
PROCEDURE pLocalCSV:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphQuery AS HANDLE NO-UNDO.

    DEFINE VARIABLE cBufferValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCompany       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustListField AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelFile     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hDynCalcField  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hTable         AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iColumn        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE hOutputProcs   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hQuery         AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hQueryBuf      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iNumResults    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lProceed       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lUseCustList   AS LOGICAL   NO-UNDO.

    SESSION:SET-WAIT-STATE("General").
    RUN AOA/spDynCalcField.p PERSISTENT SET hDynCalcField.
    RUN system/OutputProcs.p PERSISTENT SET hOutputProcs.
    RUN spGetSessionParam ("Company", OUTPUT cCompany). 

    IF dynParamValue.useCustList OR dynParamValue.CustListID NE "" THEN
    RUN spCustList (
        dynParamValue.subjectID,
        dynParamValue.user-id,
        dynParamValue.prgmName,
        dynParamValue.paramValueID,
        dynParamValue.custListID,
        OUTPUT cCustListField,
        OUTPUT lUseCustList
        ).
    OS-CREATE-DIR "users".
    OS-CREATE-DIR VALUE("users\" + aoaUserID).
    cExcelFile = "users\" + aoaUserID + "\"
               + REPLACE(aoaTitle," ","") + "."
               + STRING(TODAY,"99999999") + "."
               + STRING(TIME,"99999")
               + ".csv"
               .
    IF SEARCH(cExcelFile) NE ? THEN
    OS-DELETE VALUE(SEARCH(cExcelFile)).
    OUTPUT STREAM sLocalCSV TO VALUE(cExcelFile).
    IF svShowPageHeader THEN DO:
        FOR EACH dynValueColumn NO-LOCK
            WHERE dynValueColumn.subjectID    EQ dynParamValue.subjectID
              AND dynValueColumn.user-id      EQ dynParamValue.user-id
              AND dynValueColumn.prgmName     EQ dynParamValue.prgmName
              AND dynValueColumn.paramValueID EQ dynParamValue.paramValueID
              AND dynValueColumn.isActive     EQ YES
               BY dynValueColumn.sortOrder
            :
            PUT STREAM sLocalCSV UNFORMATTED dynValueColumn.colLabel + ",".
        END. /* each dynvaluecolumn */
        PUT STREAM sLocalCSV UNFORMATTED SKIP.
    END. /* show page header */
    iphQuery:QUERY-OPEN.
    {AOA/includes/iNumResults.i}
    iphQuery:GET-FIRST().
    IF NOT iphQuery:QUERY-OFF-END THEN
    REPEAT:
        ASSIGN
            lProceed = YES
            idx = idx + 1
            .
        RUN spProgressBar (aoaTitle, idx, iNumResults).
        IF lUseCustList THEN
        RUN spCheckCustList (iphQuery, cCustListField, OUTPUT lProceed).
        IF lProceed THEN DO:
            FOR EACH dynValueColumn NO-LOCK
                WHERE dynValueColumn.subjectID    EQ dynParamValue.subjectID
                  AND dynValueColumn.user-id      EQ dynParamValue.user-id
                  AND dynValueColumn.prgmName     EQ dynParamValue.prgmName
                  AND dynValueColumn.paramValueID EQ dynParamValue.paramValueID
                  AND dynValueColumn.isActive     EQ YES
                   BY dynValueColumn.sortOrder
                :
                IF dynValueColumn.isCalcField THEN DO:
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
                    cBufferValue = DYNAMIC-FUNCTION("FormatForCSV" IN hOutputProcs, cBufferValue)
                    .
                IF cBufferValue EQ ? THEN
                cBufferValue = "".
                PUT STREAM sLocalCSV UNFORMATTED REPLACE(cBufferValue,",","") + ",".
            END. /* each dynvaluecolumn */
            PUT STREAM sLocalCSV UNFORMATTED SKIP.
        END. /* if lproceed */
        iphQuery:GET-NEXT().
        IF iphQuery:QUERY-OFF-END THEN LEAVE.
    END. /* repeat */
    iphQuery:QUERY-CLOSE().
    DELETE OBJECT iphQuery.
    OUTPUT STREAM sLocalCSV CLOSE.
    DELETE PROCEDURE hDynCalcField.
    DELETE PROCEDURE hOutputProcs.  
    OS-COMMAND NO-WAIT START excel.exe VALUE("~"" + cExcelFile + "~"").
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-pSetColumnOrder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetColumnOrder Procedure 
PROCEDURE pSetColumnOrder :
/*------------------------------------------------------------------------------
 Purpose: intentionally empty, do not remove
 Notes:
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pSubDataSetRecordsExist) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSubDataSetRecordsExist Procedure
PROCEDURE pSubDataSetRecordsExist:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTables    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTypes     AS CHARACTER NO-UNDO INITIAL "Detail,Summary".
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTables    AS INTEGER   NO-UNDO.

    DEFINE VARIABLE cHandle    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hQuery     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hTable     AS HANDLE    NO-UNDO.

    DO idx = 1 TO NUM-ENTRIES(cTypes):
        RUN spGetSessionParam (ENTRY(idx,cTypes) + "Tables", OUTPUT cTables).
        iTables = INTEGER(cTables).
        IF iTables EQ 0 THEN NEXT.
        DO jdx = 1 TO iTables:
            RUN spGetSessionParam (ENTRY(idx,cTypes) + "Handle" + STRING(jdx), OUTPUT cHandle).
            hTable = WIDGET-HANDLE(cHandle).
            IF NOT VALID-HANDLE(hTable) THEN NEXT.
            hTable = hTable:DEFAULT-BUFFER-HANDLE.
            CREATE QUERY hQuery.
            hQuery:SET-BUFFERS(hTable:HANDLE).
            hQuery:QUERY-PREPARE("FOR EACH " + hTable:NAME).
            hQuery:QUERY-OPEN.
            hQuery:GET-FIRST().
            IF hQuery:QUERY-OFF-END THEN NEXT.
            CASE ENTRY(idx,cTypes):
                WHEN "Detail" THEN
                lDetailDataSet[jdx] = YES.
                WHEN "Summary" THEN
                lSummaryDataSet[jdx] = YES.
            END CASE.
            DELETE OBJECT hQuery.
        END. /* do jdx */
    END. /* do idx */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-spJasper) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spJasper Procedure 
PROCEDURE spJasper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcType       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iprRowID      AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER iphAppSrv     AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER iphAppSrvBin  AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTaskRecKey AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJasperFile AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iSize AS INTEGER NO-UNDO.

    IF lJasperStarter EQ NO THEN DO:
        MESSAGE 
          "Jasper Starter is NOT installed, please contact" SKIP
          "your System Administrator for assistance."
        VIEW-AS ALERT-BOX WARNING.
        RETURN.
    END. /* if test for jasperstarter */
    ASSIGN
        hAppSrv    = iphAppSrv
        hAppSrvBin = iphAppSrvBin
        .
    /* find user-print storing parameter values */
    RUN pGetUserPrint (iprRowID).
    /* create temp-table ttColumn */
    RUN pCreateTempTableColumn.
    /* find user-print storing jasper values */
    RUN pGetJasperUserPrint.    
    /* set columns for selected report columns */
    RUN pGetSelectedColumns.
    /* calculate width of jasper report */
    iSize = fJasperReportSize().
    /* if no active columns, done */
    IF iSize EQ ? THEN RETURN.    
    /* create jasper files in local user folder */
    /* create xml data file */
    RUN pJasperJSON.
    /* create jasper jrxml file */
    cJasperFile = "users\" + aoaUserID + "\"
                + REPLACE(aoaTitle," ","") + "."
                + ipcTaskRecKey + ".jrxml"
                .    
    OUTPUT STREAM sJasper TO VALUE(cJasperFile).    
    RUN pJasperReport ("Open", ipcType, iSize).
    RUN pJasperStyles.
    RUN pJasperFieldDeclarations.
    RUN pJasperVariableDeclarations.
    IF svShowGroupHeader OR svShowGroupFooter THEN
    RUN pJasperGroupDeclarations.
    RUN pJasperBackgroundBand.    
    IF svShowReportHeader THEN
    RUN pJasterTitleBand (iSize).    
    IF svShowPageHeader THEN DO:
        RUN pJasperPageHeaderBand.    
        RUN pJasperColumnHeaderBand.
    END. /* show page header */
    /*IF svShowGroupHeader THEN*/    
    RUN pJasperDetailBand (iSize).    
    IF svShowGroupFooter THEN
    RUN pJasperColumnFooterBand.    
    IF svShowPageFooter THEN
    RUN pJasperPageFooterBand.    
    IF svShowParameters THEN
    RUN pJasperLastPageFooter ("user-print").    
    IF svShowReportFooter THEN 
    RUN pJasperSummaryBand.    
    RUN pJasperReport ("Close", ipcType, iSize).    
    OUTPUT STREAM sJasper CLOSE.    
    /* copy local jasper files to jasper studio workspace */
    RUN pJasperCopy (cJasperFile).
    /* command line call to jasperstarter script */
    RUN pJasperStarter (ipcType, "AOA", OUTPUT opcJasperFile).
    RUN pDeleteSessionParam ("Detail").
    RUN pDeleteSessionParam ("Summary").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-spJasperQuery) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spJasperQuery Procedure
PROCEDURE spJasperQuery:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcType       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iprRowID      AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTitle      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserID     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iphAppSrvBin  AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTaskRecKey AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJasperFile AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cError        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cjrxml        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTableName    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserFolder   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hQuery        AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iSize         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lOKJasperJSON AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lOK           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lProgressBar  AS LOGICAL   NO-UNDO.

    ASSIGN
        aoaTitle     = ipcTitle
        aoaUserID    = ipcUserID
        hAppSrvBin   = iphAppSrvBin
        cjrxml       = REPLACE(aoaTitle," ","") + "." + ipcTaskRecKey
        lProgressBar = CAN-DO("Grid,LocalCSV,Print -d,View", ipcType)
        .
    /* find dynParamValue storing parameter values */
    RUN pGetUserParamValue (iprRowID).
    /* set columns for selected report columns */
    RUN pGetSelectedColumns.

    /* get dynamic subject tables */
    FOR EACH dynSubjectTable
        WHERE dynSubjectTable.subjectID EQ dynParamValue.subjectID
           BY dynSubjectTable.sortOrder
        :
        cTableName = cTableName + dynSubjectTable.tableName + ",".
    END. /* each {1}SubjectTable */
    cTableName = TRIM(cTableName,",").
    FIND FIRST dynSubject NO-LOCK
         WHERE dynSubject.subjectID EQ dynParamValue.subjectID
         NO-ERROR.
    IF AVAILABLE dynSubject THEN DO:
        IF dynSubject.businessLogic EQ "" THEN
        /* create dynamic query */
        RUN AOA/dynQuery.p (
            ROWID(dynParamValue),
            dynSubject.queryStr,
            cTableName,
            dynParamValue.recordLimit, /* zero = no record limit */
            OUTPUT hQuery,
            OUTPUT lOK,
            OUTPUT cError
            ).
        ELSE
        /* run business logic */
        RUN pRunBusinessLogic (
            lProgressBar,
            OUTPUT hQuery,
            OUTPUT lOK,
            OUTPUT cError
            ).
        IF lOK THEN DO:
            /* if not local csv */
            IF ipcType EQ "LocalCSV" THEN
            RUN pLocalCSV (hQuery).
            ELSE DO:
                RUN AOA/jasperJSON.p (
                    ROWID(dynParamValue),
                    hQuery,
                    ipcUserID,
                    dynSubject.subjectTitle,
                    ipcTaskRecKey,
                    lProgressBar,
                    OUTPUT cJasperFile,
                    OUTPUT lOKJasperJSON
                    ).
                RUN pSubDataSetRecordsExist.
                /* check if using external form */
                IF dynParamValue.externalForm NE "" AND
                   SEARCH(dynParamValue.externalForm) NE ? THEN DO:
                    RUN pCreateDir (OUTPUT cUserFolder).
                    OS-COPY
                        VALUE(dynParamValue.externalForm)
                        VALUE(cUserFolder + cjrxml + ".jrxml")
                        .
                    cJasperFile = SEARCH(cUserFolder + cjrxml + ".jrxml").
                END. /* if external form */
                ELSE DO: /* dynamically create jasper report */
                    /* calculate width of jasper report */
                    iSize = fJasperReportSize().
                    /* if no active columns, done */
                    IF iSize EQ ? THEN RETURN.    
                    /* create jasper jrxml file */
                    cJasperFile = "users\" + aoaUserID + "\" + cjrxml + ".jrxml".
                    OUTPUT STREAM sJasper TO VALUE(cJasperFile).    
                    RUN pJasperReport ("Open", ipcType, iSize).
                    RUN pJasperStyles.
                    RUN pJasperFieldDeclarations.
                    RUN pJasperVariableDeclarations.
                    IF svShowGroupHeader OR svShowGroupFooter THEN
                    RUN pJasperGroupDeclarations.
                    RUN pJasperBackgroundBand.    
                    IF svShowReportHeader THEN
                    RUN pJasterTitleBand (iSize).    
                    IF svShowPageHeader THEN DO:
                        RUN pJasperPageHeaderBand.    
                        RUN pJasperColumnHeaderBand.
                    END. /* show page header */
                    /*IF svShowGroupHeader THEN*/    
                    RUN pJasperDetailBand (iSize).    
                    IF svShowGroupFooter THEN
                    RUN pJasperColumnFooterBand.    
                    IF svShowPageFooter THEN
                    RUN pJasperPageFooterBand.    
                    IF svShowParameters THEN
                    RUN pJasperLastPageFooter ("dynParamValue").
                    IF svShowReportFooter THEN 
                    RUN pJasperSummaryBand.    
                    RUN pJasperReport ("Close", ipcType, iSize).    
                    OUTPUT STREAM sJasper CLOSE.
                END. /* else if not using external form */
                /* copy local jasper files to jasper studio workspace */
                RUN pJasperCopy (cJasperFile).
                IF lOKJasperJSON THEN
                /* command line call to jasperstarter script */
                RUN pJasperStarter (ipcType, ipcTaskRecKey, OUTPUT opcJasperFile).
            END. /* if local csv */
        END. /* if lok */
    END. /* avail dynsubject */

    RUN pDeleteSessionParam ("Detail").
    RUN pDeleteSessionParam ("Summary").

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fDataType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fDataType Procedure
FUNCTION fDataType RETURNS CHARACTER 
  (ipcDataType AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDataType AS CHARACTER NO-UNDO.

    CASE ipcDataType:
        WHEN "Character" THEN
        cDataType = "String".
        WHEN "Decimal" THEN
        cDataType = "Double".
        WHEN "Integer" THEN
        cDataType = "Integer".
        OTHERWISE
        cDataType = "String".
    END CASE.
    RETURN cDataType.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fFormatValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fFormatValue Procedure 
FUNCTION fFormatValue RETURNS CHARACTER
  (iphTable AS HANDLE, ipcField AS CHARACTER, ipcFormat AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: format field value
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cStr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx  AS INTEGER   NO-UNDO.

    IF INDEX(ipcField,"[") NE 0 THEN
    ASSIGN
        cStr = SUBSTRING(ipcField,INDEX(ipcField,"[") + 1)
        cStr = REPLACE(cStr,"]","")
        idx  = INTEGER(cStr)
        ipcField = SUBSTRING(ipcField,1,INDEX(ipcField,"[") - 1)
        .
    IF ipcFormat EQ "" THEN
    ipcFormat = iphTable:BUFFER-FIELD(ipcField):FORMAT.
    cStr = STRING(iphTable:BUFFER-FIELD(ipcField):BUFFER-VALUE(idx),ipcFormat) NO-ERROR.
    /* error raised if invalid format for field value */
    IF ERROR-STATUS:NUM-MESSAGES NE 0 OR
       iphTable:BUFFER-FIELD(ipcField):DATA-TYPE EQ "CHARACTER" THEN 
    cStr = iphTable:BUFFER-FIELD(ipcField):BUFFER-VALUE(idx).
    
    RETURN LEFT-TRIM(TRIM(cStr)).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fGetModule) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetModule Procedure 
FUNCTION fGetModule RETURNS CHARACTER
  ( ipcType AS CHARACTER, ipcProgramID AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cModule    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cProgramID AS CHARACTER NO-UNDO.

    FILE-INFO:FILE-NAME = "AOA/datFiles/" + ipcType + ".dat".
    INPUT FROM VALUE(FILE-INFO:FULL-PATHNAME) NO-ECHO.
    REPEAT:
        IMPORT cModule ^ cProgramID.
        IF cProgramID EQ ipcProgramID THEN LEAVE.
        cModule = "XX".
    END. /* repeat */
    INPUT CLOSE.
    
    RETURN cModule.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fJasperCalcPattern) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fJasperCalcPattern Procedure 
FUNCTION fJasperCalcPattern RETURNS CHARACTER
  (ipcDataType AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cPattern AS CHARACTER NO-UNDO.
    
    CASE ipcDataType:
        WHEN "Integer" THEN
        cPattern = "#,##0".            
        WHEN "Double" THEN
        cPattern = "#,##0.#####".
        WHEN "String" THEN
        cPattern = "X(0)". 
    END CASE.
    
    RETURN cPattern.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fJasperPattern) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fJasperPattern Procedure 
FUNCTION fJasperPattern RETURNS CHARACTER
  (ipcFormat AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    RETURN REPLACE(REPLACE(REPLACE(REPLACE(ipcFormat,">","#"),"9","0"),"-",""),"<","#").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fJasperReportSize) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fJasperReportSize Procedure 
FUNCTION fJasperReportSize RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    FIND LAST ttColumn USE-INDEX ttOrder
         WHERE ttColumn.isActive       EQ YES
           AND ttColumn.ttJasperSize   NE 0
           AND ttColumn.ttJasperColumn NE 0
         NO-ERROR.
    IF NOT AVAILABLE ttColumn THEN RETURN ?.
    RETURN ttColumn.ttJasperColumn + ttColumn.ttJasperSize + 100.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
