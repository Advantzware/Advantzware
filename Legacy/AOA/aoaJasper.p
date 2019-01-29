&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : AOA/aoaJasper.p
    Purpose     : SUPER-PROCEDURE Jasper Functions and Procedures

    Syntax      : RUN AOA/aoaJasper.p

    Description : AOA Jasper Functions & Procedures

    Author(s)   : Ron Stark
    Created     : 12.5.2018
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
&SCOPED-DEFINE aoaJasper 7
&SCOPED-DEFINE aoaJasperGap 5
&SCOPED-DEFINE noBrowseRefresh

DEFINE VARIABLE aoaCompany         AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaProgramID       AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaBatchSeq        AS INTEGER   NO-UNDO.
DEFINE VARIABLE aoaUserID          AS CHARACTER NO-UNDO.
DEFINE VARIABLE aoaTitle           AS CHARACTER NO-UNDO.
DEFINE VARIABLE hAppSrv            AS HANDLE    NO-UNDO.
DEFINE VARIABLE hAppSrvBin         AS HANDLE    NO-UNDO.
DEFINE VARIABLE cSelectedColumns   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lJasperStarter     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lUseDefault        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE svShowParameters   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE svShowReportHeader AS LOGICAL   NO-UNDO.
DEFINE VARIABLE svShowReportFooter AS LOGICAL   NO-UNDO.
DEFINE VARIABLE svShowPageHeader   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE svShowPageFooter   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE svShowGroupHeader  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE svShowGroupFooter  AS LOGICAL   NO-UNDO.

DEFINE BUFFER jasperUserPrint FOR user-print.

{AOA/includes/ttColumn.i}
{AOA/includes/aoaProcedures.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fFormatValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fFormatValue Procedure 
FUNCTION fFormatValue RETURNS CHARACTER
  (iphTable AS HANDLE, ipcField AS CHARACTER) FORWARD.

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
&IF DEFINED(EXCLUDE-pGetSelectedColumns) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSelectedColumns Procedure 
PROCEDURE pGetSelectedColumns :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iColumn AS INTEGER NO-UNDO.
    
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
    PUT UNFORMATTED
        "    <background>" SKIP
        "        <band splitType=~"Stretch~"/>" SKIP
        "    </background>" SKIP
        .

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
    PUT UNFORMATTED
        "    <columnFooter>" SKIP
        "        <band height=~"" 14 "~" splitType=~"Stretch~">" SKIP
        .
    RUN pJasperGroupType ("Column").
    PUT UNFORMATTED
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
    PUT UNFORMATTED
        "    <columnHeader>" SKIP
        "        <band height=~"" 14 "~" splitType=~"Stretch~">" SKIP
        .
    FOR EACH ttColumn
        WHERE ttColumn.isActive EQ YES
           BY ttColumn.ttOrder
        :
        PUT UNFORMATTED
            "            <staticText>" SKIP
            "                <reportElement "
            "x=~"" ttColumn.ttJasperColumn "~" "
            "y=~"" 0 "~" "
            "width=~"" ttColumn.ttJasperSize "~" "
            "height=~"" 14 "~"/>" SKIP
            "                    <textElement"
            .
        IF CAN-DO("Decimal,Integer",ttColumn.ttType) THEN
        PUT UNFORMATTED
            " textAlignment=~"Right~""
            .
        PUT UNFORMATTED
            ">" SKIP
            "                        <font isBold=~"true~" isUnderline=~"true~"/>" SKIP
            "                    </textElement>" SKIP
            "                <text><![CDATA[" ttColumn.ttLabel "]]></text>" SKIP
            "            </staticText>" SKIP
            .
        END. /* each ttColumn */
    PUT UNFORMATTED
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

    DEFINE BUFFER ttColumn FOR ttColumn.
    
    /* detail band */
    PUT UNFORMATTED
        "    <detail>" SKIP
        "        <band height=~"" 14 "~" splitType=~"Stretch~">" SKIP
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
        .
    FOR EACH ttColumn
        WHERE ttColumn.isActive EQ YES
           BY ttColumn.ttOrder
        :
        PUT UNFORMATTED
            "            <textField isBlankWhenNull=~"true~""
            .
        IF CAN-DO("Decimal,Integer",ttColumn.ttType) THEN
        PUT UNFORMATTED
            " pattern=~"" fJasperPattern(ttColumn.ttFormat) "~""
            .
        PUT UNFORMATTED
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
        PUT UNFORMATTED
            "                <textElement textAlignment=~"Right~"/>" SKIP
            .
        PUT UNFORMATTED
            "                <textFieldExpression><![CDATA[$F~{" ttColumn.ttField
            "}]]></textFieldExpression>" SKIP
            "            </textField>" SKIP
            .
    END. /* each ttColumn */
    PUT UNFORMATTED
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
    DEFINE VARIABLE cDataType AS CHARACTER NO-UNDO.

    DEFINE BUFFER ttColumn FOR ttColumn.
    
    /* field declarations */
    FOR EACH ttColumn
        WHERE ttColumn.isActive    EQ YES
           OR ttColumn.isGroup     EQ YES
           OR ttColumn.ttGroupCalc NE ""
           BY ttColumn.ttOrder
        :
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
        PUT UNFORMATTED
            "    <field name=~"" ttColumn.ttField "~" class=~"java.lang." cDataType "~">" SKIP
            "        <property name=~"net.sf.jasperreports.xpath.field.expression~" value=~"" ttColumn.ttField "~"/>" SKIP
            "        <fieldDescription><![CDATA[" ttColumn.ttField "]]></fieldDescription>" SKIP
            "    </field>" SKIP
            .
    END. /* each ttColumn */

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
    DEFINE BUFFER ttColumn FOR ttColumn.
    
    /* groups declarations */
    FOR EACH ttColumn
        WHERE ttColumn.isGroup EQ YES
           BY ttColumn.ttOrder
        :
        PUT UNFORMATTED
            "    <group name=~"" REPLACE(ttColumn.ttLabel," ","_") + "_Group~">" SKIP
            "        <groupExpression><![CDATA[$F~{" ttColumn.ttField "}]]></groupExpression>" SKIP
            .
        IF svShowGroupHeader THEN
        RUN pJasperGroupHeader (ROWID(ttColumn)).
        IF svShowGroupFooter THEN
        RUN pJasperGroupFooter (ROWID(ttColumn)).
        PUT UNFORMATTED 
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
    PUT UNFORMATTED
        "        <groupFooter>" SKIP
        "            <band height=~"" 20 "~" splitType=~"Stretch~">" SKIP
        "                <staticText>" SKIP
        "                    <reportElement "
        "x=~"" 0 "~" "
        "y=~"" 0 "~" "
        "width=~"" (LENGTH(ttColumn.ttLabel) + 6) * {&aoaJasper} "~" "
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
        ELSE
        cPAttern = fJasperPattern(bttColumn.ttFormat).
        PUT UNFORMATTED
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
    PUT UNFORMATTED
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
    
    PUT UNFORMATTED
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
    
    DEFINE VARIABLE cPattern AS CHARACTER NO-UNDO.
    
    FOR EACH ttColumn
        WHERE ttColumn.ttGroupCalc NE "",
        EACH ttGroupCalc
        WHERE ttGroupCalc.ttField EQ ttColumn.ttField
          AND ttGroupCalc.ttGroup EQ ipcGroupType
        BREAK BY ttGroupCalc.ttGroup
              BY ttColumn.ttOrder
        :
        IF FIRST-OF(ttGroupCalc.ttGroup) THEN
        PUT UNFORMATTED
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
        ELSE
        cPattern = fJasperPattern(ttColumn.ttFormat).
        PUT UNFORMATTED
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
        PUT UNFORMATTED
            "                    <bottomPen lineWidth=~"1.0~"/>" SKIP
            .
        PUT UNFORMATTED
            "                </box>" SKIP
            "                <textElement textAlignment=~"Right~">" SKIP
            "                    <font isBold=~"true~"/>" SKIP
            "                </textElement>" SKIP
            "                <textFieldExpression><![CDATA[$V~{" ttColumn.ttField
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
  Purpose:     Export temp-table contents to XML Format
  Parameters:  user-print buffer
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER user-print FOR user-print.

    DEFINE VARIABLE hTable       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cColumns     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE fieldName    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iColumn      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE hQuery       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hQueryBuf    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cDynFunc     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJasperFile  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBufferValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFirstRow    AS LOGICAL   NO-UNDO INITIAL YES.
    
    DEFINE BUFFER ttColumn FOR ttColumn.
    
    IF VALID-HANDLE(hAppSrv) THEN DO WITH FRAME frameColumns:
        hTable = DYNAMIC-FUNCTION('fGetTableHandle' IN hAppSrv, aoaProgramID).
        IF NOT VALID-HANDLE(hTable) THEN RETURN.
        
        OS-CREATE-DIR "users".
        OS-CREATE-DIR VALUE("users\" + aoaUserID).
        cJasperFile = "users\" + aoaUserID + "\" + REPLACE(aoaTitle," ","") + ".json".
        OUTPUT TO VALUE(cJasperFile).
        PUT UNFORMATTED
            "~{" SKIP
            FILL(" ",2)
            "~"" REPLACE(aoaTitle," ","_") "~": ~{" SKIP
            FILL(" ",4)
            "~"tt" REPLACE(aoaTitle," ","") "~": [" SKIP
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
            PUT UNFORMATTED "," SKIP.
            ELSE
            lFirstRow = NO.
            PUT UNFORMATTED FILL(" ",6) "~{" SKIP.
            FOR EACH ttColumn
                WHERE ttColumn.isActive    EQ YES
                   OR ttColumn.isGroup     EQ YES
                   OR ttColumn.ttGroupCalc NE ""
                BREAK BY ttColumn.ttField:
                ASSIGN 
                    fieldName    = ttColumn.ttField
                    cBufferValue = fFormatValue(hTable, hTable:BUFFER-FIELD(fieldName):NAME)
                    /* remove special characters with escape values */
                    cBufferValue = REPLACE(cBufferValue,"~&","~&amp;")
                    cBufferValue = REPLACE(cBufferValue,"~'","~&apos;")
                    cBufferValue = REPLACE(cBufferValue,"~"","~&quot;")
                    cBufferValue = REPLACE(cBufferValue,"<","~&lt;")
                    cBufferValue = REPLACE(cBufferValue,">","~&gt;")
                    cBufferValue = REPLACE(cBufferValue,"~\","~\~\")
                    .
                PUT UNFORMATTED
                    FILL(" ",8)
                    "~"" fieldName "~": ~""
                    IF cBufferValue NE "" THEN cBufferValue ELSE " "
                    "~""
                    .
                IF NOT LAST(ttColumn.ttField) THEN
                PUT UNFORMATTED "," SKIP.
            END. /* each ttColumn */
            PUT UNFORMATTED SKIP FILL(" ",6) "}".
        END. /* repeat */
        hQuery:QUERY-CLOSE().
        DELETE OBJECT hQuery.
        PUT UNFORMATTED
            SKIP
            FILL(" ",4) "]" SKIP
            FILL(" ",2) "}" SKIP
            "}" SKIP
            .
        OUTPUT CLOSE.
        RUN pJasperCopy (cJasperFile).
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
    DEFINE VARIABLE cParameter     AS CHARACTER NO-UNDO EXTENT 100.
    DEFINE VARIABLE cValue         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtDate         AS DATE      NO-UNDO.
    DEFINE VARIABLE idx            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iParameterRow  AS INTEGER   NO-UNDO INITIAL 1.
    
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
    
    /* last page footer band */
    PUT UNFORMATTED
        "    <lastPageFooter>" SKIP
        "        <band height=~"" (iParameterRow + 3) * 14 "~" splitType=~"Stretch~">" SKIP
        .
    IF svShowPageFooter THEN
    RUN pJasperGroupType ("Page").
    PUT UNFORMATTED
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
        IF cParameter[idx] NE "" THEN
        PUT UNFORMATTED
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
    PUT UNFORMATTED
        "        </band>" SKIP
        "    </lastPageFooter>" SKIP
        .

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
    PUT UNFORMATTED
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
    PUT UNFORMATTED
        "    <pageFooter>" SKIP
        "        <band height=~"" 44 "~" splitType=~"Stretch~">" SKIP
        .
    RUN pJasperGroupType ("Page").    
    RUN pJasperPageBottom (0).
    PUT UNFORMATTED
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
    PUT UNFORMATTED
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
    PUT UNFORMATTED
        "    <queryString language=~"json~">" SKIP
        "        <![CDATA[" REPLACE(aoaTitle," ","_")
        ".tt" REPLACE(aoaTitle," ","") "]]>" SKIP
        "    </queryString>" SKIP
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
        WHEN "Open" THEN
        PUT UNFORMATTED
            "<?xml version=~"1.0~" encoding=~"UTF-8~"?>" SKIP
            "<!-- Created with Jaspersoft Studio version 6.6.0.final using JasperReports Library version 6.6.0  -->" SKIP
            "<jasperReport xmlns=~"http://jasperreports.sourceforge.net/jasperreports~" "
            "xmlns:xsi=~"http://www.w3.org/2001/XMLSchema-instance~" "
            "xsi:schemaLocation=~"http://jasperreports.sourceforge.net/jasperreports "
            "http://jasperreports.sourceforge.net/xsd/jasperreport.xsd~" "
            "name=~"" REPLACE(aoaTitle," ","") "~" "
            "pageWidth=~"" ipiSize "~" "
            "orientation=~"Landscape~" "
            "columnWidth=~"" ipiSize - 40 "~" "
            "leftMargin=~"" iMargin "~" "
            "rightMargin=~"" iMargin "~" "
            "topMargin=~"" iMargin "~" "
            "bottomMargin=~"" iMargin "~">" SKIP
            .
        WHEN "Close" THEN
        PUT UNFORMATTED
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
    DEFINE INPUT  PARAMETER ipcType     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJastFile AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cJasperStarter AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJasperFile    AS CHARACTER NO-UNDO EXTENT 4.
    DEFINE VARIABLE cJasperFolder  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserFolder    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtDate         AS DATE      NO-UNDO.
    DEFINE VARIABLE idx            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTime          AS INTEGER   NO-UNDO.
    
    /* ensure needed folders exist */
    OS-CREATE-DIR "TaskResults".
    OS-CREATE-DIR "users".
    cUserFolder = "users/" + aoaUserID + "/".
    OS-CREATE-DIR VALUE(cUserFolder).
    cJasperFolder = "users/" + aoaUserID + "/Jasper/".
    OS-CREATE-DIR VALUE(cJasperFolder).
    ASSIGN
        dtDate         = TODAY
        iTime          = TIME
        cJasperFile[1] = SEARCH(cUserFolder + REPLACE(aoaTitle," ","") + ".jrxml")
        cJasperFile[2] = SEARCH(cUserFolder + REPLACE(aoaTitle," ","") + ".json")
        cJasperFile[3] = REPLACE(cJasperFile[1],"jrxml",ipcType)
        cJasperFile[3] = REPLACE(cJasperFile[3]," -d","")
        cJasperFile[4] = "TaskResults/"
                       + REPLACE(aoaTitle," ","") + "."
                       + STRING(YEAR(dtDate),"9999")
                       + STRING(MONTH(dtDate),"99")
                       + STRING(DAY(dtDate),"99") + "."
                       + STRING(iTime,"99999")
        opcJastFile    = cJasperFile[4] + "." + LC(ipcType)
        cJasperStarter = "jasperstarter process "
                       + "-f " + LC(ipcType) + " "
                       + "-t json "
                       + "-o " + cJasperFile[4] + " "
                       + "--data-file "
                       + cJasperFile[2] + " "
                       + "--json-query "
                       + REPLACE(aoaTitle," ","_")
                       + ".tt" + REPLACE(aoaTitle," ","") + " "
                       +  cJasperFile[1]
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
    
    IF NOT CAN-DO("print -d,view",ipcType) THEN DO:
        CREATE TaskResult.
        ASSIGN
            TaskResult.fileDateTime = DATETIME(dtDate,iTime)
            TaskResult.fileType     = ipcType
            TaskResult.user-id      = aoaUserID
            TaskResult.folderFile   = opcJastFile
            .
    END. /* if not can-do */
    OS-DELETE VALUE(cJasperFile[3]).
    OS-COMMAND NO-WAIT start VALUE(cJasperStarter).

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
    
    cColor = "#FFF1D1".
    PUT UNFORMATTED
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

&IF DEFINED(EXCLUDE-pJasperSummaryBand) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperSummaryBand Procedure 
PROCEDURE pJasperSummaryBand :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* summary band */
    PUT UNFORMATTED
        "    <summary>" SKIP
        "        <band height=~"" 14 "~" splitType=~"Stretch~">" SKIP
        .
    RUN pJasperGroupType ("Report").
    PUT UNFORMATTED
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
        ASSIGN
            cResetGroup = REPLACE(REPLACE(ttGroupCalc.ttGroup,"[Group] ","")," ","_") + "_Group"
            cName       = ttGroupCalc.ttField + "_"
                        + IF ttGroupCalc.ttGroup BEGINS "[Group] " THEN cResetGroup
                          ELSE ttGroupCalc.ttGroup + "Footer"
                        .
        PUT UNFORMATTED
            "    <variable name=~"" cName "~" class=~"java.lang." cDataType
            .
        IF ttGroupCalc.ttGroup BEGINS "[Group] " THEN
        PUT UNFORMATTED
            "~" resetType=~"Group~" resetGroup=~"" cResetGroup
            .
        ELSE IF ttGroupCalc.ttGroup NE "Report" THEN
        PUT UNFORMATTED
            "~" resetType=~"" ttGroupCalc.ttGroup
            .
        IF ENTRY(1,ttGroupCalc.ttCalcType,"|") NE "Calculated" THEN
        PUT UNFORMATTED
            "~" calculation=~"" ttGroupCalc.ttCalcType
            .
        PUT UNFORMATTED 
             "~">" SKIP
            "        <variableExpression><![CDATA["
            .
        IF ENTRY(1,ttGroupCalc.ttCalcType,"|") EQ "Calculated" THEN
        PUT UNFORMATTED
            ENTRY(2,ttGroupCalc.ttCalcType,"|")
            .
        ELSE
        PUT UNFORMATTED
            "$F~{" ttColumn.ttField "}"
            .
        PUT UNFORMATTED
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
    /* title band */
    PUT UNFORMATTED
        "    <title>" SKIP
        "        <band height=~"" 40 "~" splitType=~"Stretch~">" SKIP
        "            <staticText>" SKIP
        "                <reportElement x=~"" 0 "~" y=~"" 0 "~" width=~"" 380 "~" height=~"" 40 "~"/>" SKIP
        "                <textElement>" SKIP
        "                    <font size=~"" 26 "~"/>" SKIP
        "                </textElement>" SKIP
        "                <text><![CDATA[" aoaTitle "]]></text>" SKIP
        "            </staticText>" SKIP
        "        </band>" SKIP
        "    </title>" SKIP
        .

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
    DEFINE OUTPUT PARAMETER opcJasperFile AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cJasperFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iSize       AS INTEGER   NO-UNDO.

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
    RUN pJasperJSON (BUFFER user-print).
    /* create jasper jrxml file */
    cJasperFile = "users\" + aoaUserID + "\" + REPLACE(aoaTitle," ","") + ".jrxml".    
    OUTPUT TO VALUE(cJasperFile).    
    RUN pJasperReport ("Open", ipcType, iSize).
    RUN pJasperStyles.
    RUN pJasperQueryString.
    RUN pJasperFieldDeclarations.
    RUN pJasperVariableDeclarations.
    IF svShowGroupHeader OR svShowGroupFooter THEN
    RUN pJasperGroupDeclarations.
    RUN pJasperBackgroundBand.    
    IF svShowReportHeader THEN
    RUN pJasterTitleBand.    
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
    RUN pJasperLastPageFooter.    
    IF svShowReportFooter THEN 
    RUN pJasperSummaryBand.    
    RUN pJasperReport ("Close", ipcType, iSize).    
    OUTPUT CLOSE.    
    /* copy local jasper files to jasper studio workspace */
    RUN pJasperCopy (cJasperFile).
    /* command line call to jasperstarter script */
    RUN pJasperStarter (ipcType, OUTPUT opcJasperFile).

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
    DEFINE INPUT  PARAMETER ipcTitle      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserID     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJasperFile AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cJasperFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iSize       AS INTEGER   NO-UNDO.
    
    ASSIGN
        aoaTitle           = ipcTitle
        aoaUserID          = ipcUserID
        svShowGroupHeader  = NO
        svShowGroupFooter  = NO
        svShowReportHeader = YES
        svShowReportFooter = YES
        svShowPageHeader   = YES
        svShowPageFooter   = YES
        svShowParameters   = NO
        .
    /* set columns for selected report columns */
    RUN pGetSelectedColumns.
    /* calculate width of jasper report */
    iSize = fJasperReportSize().
    /* if no active columns, done */
    IF iSize EQ ? THEN RETURN.    
    /* create jasper jrxml file */
    cJasperFile = "users\" + aoaUserID + "\" + REPLACE(aoaTitle," ","") + ".jrxml".    
    OUTPUT TO VALUE(cJasperFile).    
    RUN pJasperReport ("Open", ipcType, iSize).
    RUN pJasperStyles.
    RUN pJasperQueryString.
    RUN pJasperFieldDeclarations.
    RUN pJasperVariableDeclarations.
    IF svShowGroupHeader OR svShowGroupFooter THEN
    RUN pJasperGroupDeclarations.
    RUN pJasperBackgroundBand.    
    IF svShowReportHeader THEN
    RUN pJasterTitleBand.    
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
    RUN pJasperLastPageFooter.    
    IF svShowReportFooter THEN 
    RUN pJasperSummaryBand.    
    RUN pJasperReport ("Close", ipcType, iSize).    
    OUTPUT CLOSE.    
    /* copy local jasper files to jasper studio workspace */
    RUN pJasperCopy (cJasperFile).
    /* command line call to jasperstarter script */
    RUN pJasperStarter (ipcType, OUTPUT opcJasperFile).

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fFormatValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fFormatValue Procedure 
FUNCTION fFormatValue RETURNS CHARACTER
  (iphTable AS HANDLE, ipcField AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: format field value
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cStr AS CHARACTER NO-UNDO.

    cStr = STRING(iphTable:BUFFER-FIELD(ipcField):BUFFER-VALUE(),
                  iphTable:BUFFER-FIELD(ipcField):FORMAT) NO-ERROR.
    /* error raised if invalid format for field value */
    IF ERROR-STATUS:NUM-MESSAGES NE 0 OR
       iphTable:BUFFER-FIELD(ipcField):DATA-TYPE EQ "CHARACTER" THEN 
    cStr = iphTable:BUFFER-FIELD(ipcField):BUFFER-VALUE().
    
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

