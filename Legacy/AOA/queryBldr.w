&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: AOA/queryBldr.w

  Description: Dynamic Query Builder

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 12.19.2018

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

&Scoped-define program-id queryBldr.
&Scoped-define transPanel btnUpdate btnAdd btnCopy btnDelete btnReset btnCancel
&Scoped-define transInit btnUpdate btnAdd btnCopy btnDelete
&Scoped-define transUpdate btnUpdate btnReset btnCancel
&Scoped-define parameterSection RECT-PARAMETER ~
parameterSearch parameterMatches queryParameterBrowse ~
cParamNameLabel cParamName ~
cParamDataTypeLabel cParamDataType ~
cParamFormatLabel cParamFormat ~
cParamInitLabel cParamInit btnAddParamName

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cDataType          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFormat            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLabel             AS CHARACTER NO-UNDO.
DEFINE VARIABLE hJasper            AS HANDLE    NO-UNDO.
DEFINE VARIABLE hQueryBrowse       AS HANDLE    NO-UNDO.
DEFINE VARIABLE hQueryTempTable    AS HANDLE    NO-UNDO.
DEFINE VARIABLE i                  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iOrder             AS INTEGER   NO-UNDO.
DEFINE VARIABLE iUserSecurityLevel AS INTEGER   NO-UNDO.
DEFINE VARIABLE lContinue          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lSave              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE rQueryRowID        AS ROWID     NO-UNDO.
DEFINE VARIABLE rRowID             AS ROWID     NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{methods/defines/sortByDefs.i}

DEFINE TEMP-TABLE ttTable NO-UNDO
    FIELD tableName AS CHARACTER FORMAT "x(20)" LABEL "Table"
    FIELD tableDB   AS CHARACTER FORMAT "x(10)" LABEL "Database"
    FIELD tableDscr AS CHARACTER FORMAT "x(50)" LABEL "Table Description"
        INDEX ttTableName IS PRIMARY tableName
        INDEX ttTableDB tableDB
        .
DEFINE TEMP-TABLE ttField NO-UNDO
    FIELD fieldName  AS CHARACTER FORMAT "x(31)" LABEL "Field Name"
    FIELD fieldLabel AS CHARACTER FORMAT "x(31)" LABEL "Field Label"
    FIELD tableDB    AS CHARACTER
        INDEX ttFieldName IS PRIMARY fieldName
        .
DEFINE TEMP-TABLE ttQueryName NO-UNDO
    FIELD queryName     AS CHARACTER FORMAT "x(32)" LABEL "Query"
    FIELD isActive      AS LOGICAL                  LABEL "Active" INITIAL YES
    FIELD queryType     AS CHARACTER FORMAT "x(8)"  LABEL "Type"   INITIAL "User"
    FIELD module        AS CHARACTER FORMAT "x(8)"  LABEL "Module"
    FIELD user-id       AS CHARACTER FORMAT "x(10)" LABEL "User ID"
    FIELD securityLevel AS INTEGER   FORMAT ">>>9"  LABEL "SecLevel"
    FIELD queryID       AS INTEGER   FORMAT ">>>9"  LABEL "QueryID"
    FIELD queryStr      AS CHARACTER
        INDEX ttQueryName IS PRIMARY queryName
        INDEX ttQueryID queryID
        .
DEFINE TEMP-TABLE ttQueryTable NO-UNDO
    FIELD queryID   AS INTEGER
    FIELD sortOrder AS INTEGER
    FIELD tableFind AS CHARACTER FORMAT "x(5)"  LABEL "Find" INITIAL "EACH"
    FIELD tableName AS CHARACTER FORMAT "x(20)" LABEL "Table"
    FIELD tableDscr AS CHARACTER FORMAT "x(50)" LABEL "Table Description"
    FIELD tableDB   AS CHARACTER
        INDEX ttQueryID IS PRIMARY queryID
        .
DEFINE TEMP-TABLE ttQueryWhere NO-UNDO
    FIELD queryID      AS INTEGER
    FIELD sortOrder    AS INTEGER
    FIELD tableName    AS CHARACTER
    FIELD whereTable   AS CHARACTER FORMAT "x(20)" LABEL "Where Table"
    FIELD fieldLabel   AS CHARACTER FORMAT "x(31)" LABEL "Field Label"
    FIELD whereElement AS CHARACTER FORMAT "x(30)" LABEL "Where Element"
    FIELD dataType     AS CHARACTER FORMAT "x(9)"  LABEL "Data Type"
        INDEX ttQueryID IS PRIMARY queryID
        .
DEFINE TEMP-TABLE ttQuerySort NO-UNDO
    FIELD queryID    AS INTEGER
    FIELD sortOrder  AS INTEGER
    FIELD tableName  AS CHARACTER
    FIELD fieldName  AS CHARACTER
    FIELD sortAsc    AS LOGICAL   INITIAL YES
    FIELD fieldLabel AS CHARACTER FORMAT "x(31)" LABEL "Field Label"
    FIELD sortBy     AS CHARACTER FORMAT "x(42)" LABEL "Sort By"
        INDEX ttQueryID IS PRIMARY queryID
        .
DEFINE TEMP-TABLE ttQueryColumn NO-UNDO
    FIELD queryID     AS INTEGER
    FIELD sortOrder   AS INTEGER
    FIELD fieldName   AS CHARACTER FORMAT "x(23)" LABEL "Column"
    FIELD fieldLabel  AS CHARACTER FORMAT "x(22)" LABEL "Label"
    FIELD fieldFormat AS CHARACTER FORMAT "x(26)" LABEL "Format"
    FIELD tableName   AS CHARACTER
    FIELD columnSize  AS DECIMAL
    FIELD dataType    AS CHARACTER
    FIELD tableDB     AS CHARACTER
        INDEX ttQueryID IS PRIMARY queryID
        .
DEFINE TEMP-TABLE ttQueryParameter NO-UNDO
    FIELD queryID     AS INTEGER
    FIELD paramName   AS CHARACTER FORMAT "x(30)" LABEL "Parameter"
    FIELD dataType    AS CHARACTER FORMAT "x(10)" LABEL "Data Type"
    FIELD paramFormat AS CHARACTER FORMAT "x(26)" LABEL "Format"
    FIELD initValue   AS CHARACTER FORMAT "x(26)" LABEL "Initial Value"
    FIELD paramPrompt AS LOGICAL                  LABEL "Prompt" INITIAL YES
        INDEX ttParamName IS PRIMARY paramName
        .
RUN AOA\aoaJasper.p PERSISTENT SET hJasper.
SESSION:ADD-SUPER-PROCEDURE (hJasper).

{methods/lockWindowUpdate.i}

iUserSecurityLevel = DYNAMIC-FUNCTION("sfUserSecurityLevel").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME fieldBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttField ttQueryColumn ttQueryName ~
ttQueryParameter ttQuerySort ttQueryTable ttQueryWhere ttTable

/* Definitions for BROWSE fieldBrowse                                   */
&Scoped-define FIELDS-IN-QUERY-fieldBrowse ttField.fieldName ttField.fieldLabel   
&Scoped-define ENABLED-FIELDS-IN-QUERY-fieldBrowse   
&Scoped-define SELF-NAME fieldBrowse
&Scoped-define QUERY-STRING-fieldBrowse FOR EACH ttField WHERE (fieldMatches EQ NO AND (ttField.fieldName BEGINS fieldSearch OR ttField.fieldLabel BEGINS fieldSearch)) OR (fieldMatches EQ YES AND (ttField.fieldName MATCHES "*" + fieldSearch + "*" OR ttField.fieldLabel MATCHES "*" + fieldSearch + "*"))  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-fieldBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttField WHERE (fieldMatches EQ NO AND (ttField.fieldName BEGINS fieldSearch OR ttField.fieldLabel BEGINS fieldSearch)) OR (fieldMatches EQ YES AND (ttField.fieldName MATCHES "*" + fieldSearch + "*" OR ttField.fieldLabel MATCHES "*" + fieldSearch + "*"))  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-fieldBrowse ttField
&Scoped-define FIRST-TABLE-IN-QUERY-fieldBrowse ttField


/* Definitions for BROWSE queryColumnBrowse                             */
&Scoped-define FIELDS-IN-QUERY-queryColumnBrowse ttQueryColumn.fieldName ttQueryColumn.fieldLabel ttQueryColumn.fieldFormat   
&Scoped-define ENABLED-FIELDS-IN-QUERY-queryColumnBrowse ttQueryColumn.fieldLabel ttQueryColumn.fieldFormat   
&Scoped-define ENABLED-TABLES-IN-QUERY-queryColumnBrowse ttQueryColumn
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-queryColumnBrowse ttQueryColumn
&Scoped-define SELF-NAME queryColumnBrowse
&Scoped-define QUERY-STRING-queryColumnBrowse FOR EACH ttQueryColumn WHERE ttQueryColumn.queryID EQ ttQueryName.queryID   AND ((columnMatches EQ NO  AND ttQueryColumn.fieldName BEGINS columnSearch)    OR (columnMatches EQ YES AND ttQueryColumn.fieldName MATCHES "*" + columnSearch + "*")) BY ttQueryColumn.sortOrder
&Scoped-define OPEN-QUERY-queryColumnBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttQueryColumn WHERE ttQueryColumn.queryID EQ ttQueryName.queryID   AND ((columnMatches EQ NO  AND ttQueryColumn.fieldName BEGINS columnSearch)    OR (columnMatches EQ YES AND ttQueryColumn.fieldName MATCHES "*" + columnSearch + "*")) BY ttQueryColumn.sortOrder.
&Scoped-define TABLES-IN-QUERY-queryColumnBrowse ttQueryColumn
&Scoped-define FIRST-TABLE-IN-QUERY-queryColumnBrowse ttQueryColumn


/* Definitions for BROWSE queryNameBrowse                               */
&Scoped-define FIELDS-IN-QUERY-queryNameBrowse ttQueryName.queryName ttQueryName.isActive ttQueryName.queryID ttQueryName.queryType ttQueryName.module ttQueryName.user-id ttQueryName.securityLevel   
&Scoped-define ENABLED-FIELDS-IN-QUERY-queryNameBrowse   
&Scoped-define SELF-NAME queryNameBrowse
&Scoped-define QUERY-STRING-queryNameBrowse FOR EACH ttQueryName WHERE (queryMatches EQ NO  AND ttQueryName.queryName BEGINS querySearch)    OR (queryMatches EQ YES AND ttQueryName.queryName MATCHES "*" + querySearch + "*")
&Scoped-define OPEN-QUERY-queryNameBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttQueryName WHERE (queryMatches EQ NO  AND ttQueryName.queryName BEGINS querySearch)    OR (queryMatches EQ YES AND ttQueryName.queryName MATCHES "*" + querySearch + "*").
&Scoped-define TABLES-IN-QUERY-queryNameBrowse ttQueryName
&Scoped-define FIRST-TABLE-IN-QUERY-queryNameBrowse ttQueryName


/* Definitions for BROWSE queryParameterBrowse                          */
&Scoped-define FIELDS-IN-QUERY-queryParameterBrowse ttQueryParameter.paramName ttQueryParameter.dataType ttQueryParameter.paramFormat ttQueryParameter.initValue ttQueryParameter.paramPrompt   
&Scoped-define ENABLED-FIELDS-IN-QUERY-queryParameterBrowse ttQueryParameter.paramPrompt   
&Scoped-define ENABLED-TABLES-IN-QUERY-queryParameterBrowse ~
ttQueryParameter
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-queryParameterBrowse ttQueryParameter
&Scoped-define SELF-NAME queryParameterBrowse
&Scoped-define QUERY-STRING-queryParameterBrowse FOR EACH ttQueryParameter WHERE ttQueryParameter.queryID EQ ttQueryName.queryID   AND ((parameterMatches EQ NO  AND ttQueryParameter.paramName BEGINS parameterSearch)    OR (parameterMatches EQ YES AND ttQueryParameter.paramName MATCHES "*" + parameterSearch + "*"))
&Scoped-define OPEN-QUERY-queryParameterBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttQueryParameter WHERE ttQueryParameter.queryID EQ ttQueryName.queryID   AND ((parameterMatches EQ NO  AND ttQueryParameter.paramName BEGINS parameterSearch)    OR (parameterMatches EQ YES AND ttQueryParameter.paramName MATCHES "*" + parameterSearch + "*")).
&Scoped-define TABLES-IN-QUERY-queryParameterBrowse ttQueryParameter
&Scoped-define FIRST-TABLE-IN-QUERY-queryParameterBrowse ttQueryParameter


/* Definitions for BROWSE querySortBrowse                               */
&Scoped-define FIELDS-IN-QUERY-querySortBrowse ttQuerySort.fieldLabel ttQuerySort.sortBy   
&Scoped-define ENABLED-FIELDS-IN-QUERY-querySortBrowse   
&Scoped-define SELF-NAME querySortBrowse
&Scoped-define QUERY-STRING-querySortBrowse FOR EACH ttQuerySort WHERE ttQuerySort.queryID EQ ttQueryName.queryID BY ttQuerySort.sortOrder
&Scoped-define OPEN-QUERY-querySortBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttQuerySort WHERE ttQuerySort.queryID EQ ttQueryName.queryID BY ttQuerySort.sortOrder.
&Scoped-define TABLES-IN-QUERY-querySortBrowse ttQuerySort
&Scoped-define FIRST-TABLE-IN-QUERY-querySortBrowse ttQuerySort


/* Definitions for BROWSE queryTableBrowse                              */
&Scoped-define FIELDS-IN-QUERY-queryTableBrowse ttQueryTable.tableFind ttQueryTable.tableName ttQueryTable.tableDscr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-queryTableBrowse   
&Scoped-define SELF-NAME queryTableBrowse
&Scoped-define QUERY-STRING-queryTableBrowse FOR EACH ttQueryTable WHERE ttQueryTable.queryID EQ ttQueryName.queryID BY ttQueryTable.sortOrder
&Scoped-define OPEN-QUERY-queryTableBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttQueryTable WHERE ttQueryTable.queryID EQ ttQueryName.queryID BY ttQueryTable.sortOrder.
&Scoped-define TABLES-IN-QUERY-queryTableBrowse ttQueryTable
&Scoped-define FIRST-TABLE-IN-QUERY-queryTableBrowse ttQueryTable


/* Definitions for BROWSE queryWhereBrowse                              */
&Scoped-define FIELDS-IN-QUERY-queryWhereBrowse ttQueryWhere.whereElement ttQueryWhere.fieldLabel ttQueryWhere.dataType   
&Scoped-define ENABLED-FIELDS-IN-QUERY-queryWhereBrowse   
&Scoped-define SELF-NAME queryWhereBrowse
&Scoped-define QUERY-STRING-queryWhereBrowse FOR EACH ttQueryWhere WHERE ttQueryWhere.queryID EQ ttQueryName.queryID AND ttQueryWhere.whereTable EQ tableList BY ttQueryWhere.sortOrder
&Scoped-define OPEN-QUERY-queryWhereBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttQueryWhere WHERE ttQueryWhere.queryID EQ ttQueryName.queryID AND ttQueryWhere.whereTable EQ tableList BY ttQueryWhere.sortOrder.
&Scoped-define TABLES-IN-QUERY-queryWhereBrowse ttQueryWhere
&Scoped-define FIRST-TABLE-IN-QUERY-queryWhereBrowse ttQueryWhere


/* Definitions for BROWSE tableBrowse                                   */
&Scoped-define FIELDS-IN-QUERY-tableBrowse ttTable.tableName ttTable.tableDB ttTable.tableDscr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-tableBrowse   
&Scoped-define SELF-NAME tableBrowse
&Scoped-define QUERY-STRING-tableBrowse FOR EACH ttTable WHERE (tableMatches EQ NO AND (ttTable.tableName BEGINS tableSearch OR ttTable.tableDscr BEGINS tableSearch)) OR (tableMatches EQ YES AND (ttTable.tableName MATCHES "*" + tableSearch + "*" OR ttTable.tableDscr MATCHES "*" + tableSearch + "*"))  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-tableBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttTable WHERE (tableMatches EQ NO AND (ttTable.tableName BEGINS tableSearch OR ttTable.tableDscr BEGINS tableSearch)) OR (tableMatches EQ YES AND (ttTable.tableName MATCHES "*" + tableSearch + "*" OR ttTable.tableDscr MATCHES "*" + tableSearch + "*"))  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-tableBrowse ttTable
&Scoped-define FIRST-TABLE-IN-QUERY-tableBrowse ttTable


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-fieldBrowse}~
    ~{&OPEN-QUERY-queryColumnBrowse}~
    ~{&OPEN-QUERY-queryNameBrowse}~
    ~{&OPEN-QUERY-queryParameterBrowse}~
    ~{&OPEN-QUERY-querySortBrowse}~
    ~{&OPEN-QUERY-queryTableBrowse}~
    ~{&OPEN-QUERY-queryWhereBrowse}~
    ~{&OPEN-QUERY-tableBrowse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnAddParameter querySection btnNow btnToday ~
btnTime btnDateTime querySearch queryMatches tableSearch tableMatches ~
tableList btnOF tableListOf btnWhere btnMatches queryNameBrowse tableBrowse ~
queryWhereBrowse btnBegins btnAND btnOR btnYes btnNo btnEQ btnNE ~
fieldSearch fieldMatches sortType btnLT btnGT fieldBrowse querySortBrowse ~
btnLE btnGE btnPlus btnMinus btnMultiply btnDivide btnDate columnSearch ~
columnMatches btnDec queryColumnBrowse btnInt findType btnStr ~
queryTableBrowse btnSubstr cParameter btnOpen btnClose cConstant btnPeriod ~
btnDouble btnComma btnSingle cParamName queryStr cParamDataType ~
parameterSearch parameterMatches cParamFormat queryParameterBrowse ~
cParamInit btnAddParamName btnCloseResults btnResults btnSave ~
btnSaveResults btnRemoveSelection btnSyntax btnAddSelections btnMoveDown ~
btnAddConstant btnMoveUp btnRemove btnHTML btnView btnPDF btnDOCX btnCSV ~
btnXLS btnAdd btnCopy btnDelete btnUpdate cParameterLabel cConstantLabel ~
cParamNameLabel cParamDataTypeLabel cParamFormatLabel cParamInitLabel 
&Scoped-Define DISPLAYED-OBJECTS querySection querySearch queryMatches ~
tableSearch tableMatches tableList tableListOf fieldSearch fieldMatches ~
sortType columnSearch columnMatches findType cParameter cConstant ~
cParamName queryStr cParamDataType parameterSearch parameterMatches ~
cParamFormat cParamInit cParameterLabel cConstantLabel queryText ~
cParamNameLabel cParamDataTypeLabel cParamFormatLabel cParamInitLabel 

/* Custom List Definitions                                              */
/* allSection,tableSection,whereSection,sortSection,columnsSection,querySection */
&Scoped-define allSection btnAddParameter RECT-TABLE RECT-FIELD ~
RECT-QUERYTABLE RECT-QUERYSORT RECT-QUERYSTR RECT-COLUMN RECT-PANEL ~
RECT-JASPER RECT-PARAMETER RECT-SAVE btnNow btnToday btnTime btnDateTime ~
tableSearch tableMatches tableList btnOF tableListOf btnWhere btnMatches ~
tableBrowse queryWhereBrowse btnBegins btnAND btnOR btnYes btnNo btnEQ ~
btnNE fieldSearch fieldMatches sortType btnLT btnGT fieldBrowse ~
querySortBrowse btnLE btnGE btnPlus btnMinus btnMultiply btnDivide btnDate ~
columnSearch columnMatches btnDec queryColumnBrowse btnInt findType btnStr ~
queryTableBrowse btnSubstr cParameter btnOpen btnClose cConstant btnPeriod ~
btnDouble btnComma btnSingle cParamName queryStr cParamDataType ~
parameterSearch parameterMatches cParamFormat queryParameterBrowse ~
cParamInit btnAddParamName btnCloseResults btnResults btnSave ~
btnSaveResults btnRemoveSelection btnSyntax btnAddSelections btnMoveDown ~
btnAddConstant btnCancel btnMoveUp btnRemove btnHTML btnView btnPDF btnDOCX ~
btnCSV btnXLS btnAdd btnCopy btnDelete btnReset btnUpdate cParameterLabel ~
cConstantLabel queryText cParamNameLabel cParamDataTypeLabel ~
cParamFormatLabel cParamInitLabel 
&Scoped-define tableSection RECT-TABLE RECT-QUERYTABLE RECT-QUERYSTR ~
RECT-JASPER tableSearch tableMatches tableBrowse findType queryTableBrowse ~
queryStr btnResults btnRemoveSelection btnSyntax btnAddSelections ~
btnMoveDown btnMoveUp btnRemove btnHTML btnView btnPDF btnDOCX btnCSV ~
btnXLS queryText 
&Scoped-define whereSection btnAddParameter RECT-FIELD RECT-QUERYSTR ~
RECT-JASPER btnNow btnToday btnTime btnDateTime tableList btnOF tableListOf ~
btnWhere btnMatches queryWhereBrowse btnBegins btnAND btnOR btnYes btnNo ~
btnEQ btnNE fieldSearch fieldMatches btnLT btnGT fieldBrowse btnLE btnGE ~
btnPlus btnMinus btnMultiply btnDivide btnDate btnDec btnInt btnStr ~
btnSubstr cParameter btnOpen btnClose cConstant btnPeriod btnDouble ~
btnComma btnSingle queryStr btnResults btnRemoveSelection btnSyntax ~
btnAddSelections btnMoveDown btnAddConstant btnMoveUp btnRemove btnHTML ~
btnView btnPDF btnDOCX btnCSV btnXLS cParameterLabel cConstantLabel ~
queryText 
&Scoped-define sortSection RECT-FIELD RECT-QUERYSORT RECT-QUERYSTR ~
RECT-JASPER fieldSearch fieldMatches sortType fieldBrowse querySortBrowse ~
queryStr btnResults btnRemoveSelection btnSyntax btnAddSelections ~
btnMoveDown btnMoveUp btnRemove btnHTML btnView btnPDF btnDOCX btnCSV ~
btnXLS queryText 
&Scoped-define columnsSection RECT-FIELD RECT-COLUMN RECT-JASPER ~
fieldSearch fieldMatches fieldBrowse columnSearch columnMatches ~
queryColumnBrowse btnResults btnRemoveSelection btnAddSelections ~
btnMoveDown btnMoveUp btnRemove btnHTML btnView btnPDF btnDOCX btnCSV ~
btnXLS 
&Scoped-define querySection RECT-PANEL RECT-JASPER btnResults btnCancel ~
btnHTML btnView btnPDF btnDOCX btnCSV btnXLS btnAdd btnCopy btnDelete ~
btnReset btnUpdate 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fFormatValue C-Win 
FUNCTION fFormatValue RETURNS CHARACTER
  (iphTable AS HANDLE, ipcField AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fQueryStr C-Win 
FUNCTION fQueryStr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fSetSaveButton C-Win 
FUNCTION fSetSaveButton RETURNS LOGICAL
  (iplSave AS LOGICAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fShowQuery C-Win 
FUNCTION fShowQuery RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fSortBy C-Win 
FUNCTION fSortBy RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAdd 
     IMAGE-UP FILE "Graphics/32x32/navigate_plus.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_plus_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Add" 
     SIZE 8 BY 1.91 TOOLTIP "Add".

DEFINE BUTTON btnAddConstant 
     IMAGE-UP FILE "Graphics/16x16/navigate_plus.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Add Constant" 
     SIZE 5 BY 1.05 TOOLTIP "Add Constant".

DEFINE BUTTON btnAddParameter 
     IMAGE-UP FILE "Graphics/16x16/navigate_plus.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Add Parameter" 
     SIZE 5 BY 1.05 TOOLTIP "Add Parameter".

DEFINE BUTTON btnAddParamName 
     IMAGE-UP FILE "Graphics/16x16/navigate_plus.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Add Parameter Name" 
     SIZE 5 BY 1.05 TOOLTIP "Add Parameter Name".

DEFINE BUTTON btnAddSelections 
     IMAGE-UP FILE "Graphics/16x16/next.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Add Selection" 
     SIZE 4.4 BY 1 TOOLTIP "Add Selections".

DEFINE BUTTON btnAND 
     LABEL "AND" 
     SIZE 5 BY 1.05 TOOLTIP "AND".

DEFINE BUTTON btnBegins 
     LABEL "BEGINS" 
     SIZE 10 BY 1.05 TOOLTIP "BEGINS".

DEFINE BUTTON btnCancel 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_cross_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91 TOOLTIP "Cancel".

DEFINE BUTTON btnClose 
     LABEL ")" 
     SIZE 5 BY 1.05 TOOLTIP "Close Parentheses".

DEFINE BUTTON btnCloseResults 
     IMAGE-UP FILE "AOA/images/navigate_cross.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Close Results" 
     SIZE 4.4 BY 1 TOOLTIP "Close Results".

DEFINE BUTTON btnComma 
     LABEL "," 
     SIZE 2.4 BY 1.05 TOOLTIP "Comma".

DEFINE BUTTON btnCopy 
     IMAGE-UP FILE "Graphics/32x32/element_copy.ico":U
     IMAGE-INSENSITIVE FILE "Graphics\32x32\form_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Copy" 
     SIZE 8 BY 1.91 TOOLTIP "Copy".

DEFINE BUTTON btnCSV 
     IMAGE-UP FILE "AOA/images/aoaexcelcsv.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "csv" 
     SIZE 4.4 BY 1 TOOLTIP "Excel CSV".

DEFINE BUTTON btnDate 
     LABEL "DATE (" 
     SIZE 10 BY 1.05 TOOLTIP "Date".

DEFINE BUTTON btnDateTime 
     LABEL "DATETIME (" 
     SIZE 14 BY 1.05 TOOLTIP "DATETIME".

DEFINE BUTTON btnDec 
     LABEL "DEC (" 
     SIZE 10 BY 1.05 TOOLTIP "Decimal".

DEFINE BUTTON btnDelete 
     IMAGE-UP FILE "Graphics/32x32/navigate_minus.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_minus_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Delete" 
     SIZE 8 BY 1.91 TOOLTIP "Delete".

DEFINE BUTTON btnDivide 
     LABEL "/" 
     SIZE 5 BY 1.05 TOOLTIP "Divide".

DEFINE BUTTON btnDOCX 
     IMAGE-UP FILE "AOA/images/aoaword.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Word DOCX".

DEFINE BUTTON btnDouble 
     LABEL "~"" 
     SIZE 2.4 BY 1.05 TOOLTIP "Double Quote".

DEFINE BUTTON btnEQ 
     LABEL "EQ" 
     SIZE 5 BY 1.05 TOOLTIP "Equals".

DEFINE BUTTON btnGE 
     LABEL "GE" 
     SIZE 5 BY 1.05 TOOLTIP "Greater Than or Equal".

DEFINE BUTTON btnGT 
     LABEL "GT" 
     SIZE 5 BY 1.05 TOOLTIP "Greater Than".

DEFINE BUTTON btnHTML 
     IMAGE-UP FILE "AOA/images/html_tag.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "HTML".

DEFINE BUTTON btnInt 
     LABEL "INT (" 
     SIZE 10 BY 1.05 TOOLTIP "Integer".

DEFINE BUTTON btnLE 
     LABEL "LE" 
     SIZE 5 BY 1.05 TOOLTIP "Less Than or Equal".

DEFINE BUTTON btnLT 
     LABEL "LT" 
     SIZE 5 BY 1.05 TOOLTIP "Less Than".

DEFINE BUTTON btnMatches 
     LABEL "MATCHES" 
     SIZE 11 BY 1.05 TOOLTIP "MATCHES".

DEFINE BUTTON btnMinus 
     LABEL "-" 
     SIZE 5 BY 1.05 TOOLTIP "Minus".

DEFINE BUTTON btnMoveDown 
     IMAGE-UP FILE "Graphics/16x16/navigate_down.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Move Down".

DEFINE BUTTON btnMoveUp 
     IMAGE-UP FILE "Graphics/16x16/navigate_up.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Move Up".

DEFINE BUTTON btnMultiply 
     LABEL "*" 
     SIZE 5 BY 1.05 TOOLTIP "Multiply".

DEFINE BUTTON btnNE 
     LABEL "NE" 
     SIZE 5 BY 1.05 TOOLTIP "Not Equal".

DEFINE BUTTON btnNo 
     LABEL "NO" 
     SIZE 5 BY 1.05 TOOLTIP "NO".

DEFINE BUTTON btnNow 
     LABEL "NOW" 
     SIZE 9 BY 1.05 TOOLTIP "NOW".

DEFINE BUTTON btnOF 
     LABEL "OF" 
     SIZE 5 BY 1.05 TOOLTIP "OF".

DEFINE BUTTON btnOpen 
     LABEL "(" 
     SIZE 5 BY 1.05 TOOLTIP "Open Parentheses".

DEFINE BUTTON btnOR 
     LABEL "OR" 
     SIZE 5 BY 1.05 TOOLTIP "OR".

DEFINE BUTTON btnPDF 
     IMAGE-UP FILE "AOA/images/aoapdf.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "PDF".

DEFINE BUTTON btnPeriod 
     LABEL "." 
     SIZE 2.4 BY 1.05 TOOLTIP "Period".

DEFINE BUTTON btnPlus 
     LABEL "+" 
     SIZE 5 BY 1.05 TOOLTIP "Plus".

DEFINE BUTTON btnRemove 
     IMAGE-UP FILE "Graphics/16x16/navigate_cross.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Remove".

DEFINE BUTTON btnRemoveSelection 
     IMAGE-UP FILE "Graphics/16x16/previous.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Remove Selection" 
     SIZE 4.4 BY 1 TOOLTIP "Remove Selections".

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "Graphics/32x32/undo_32.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/undo_32_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91 TOOLTIP "Reset".

DEFINE BUTTON btnResults 
     IMAGE-UP FILE "AOA/images/media_play.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Results" 
     SIZE 4.4 BY 1 TOOLTIP "Query Results".

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Save" 
     SIZE 8 BY 1.91 TOOLTIP "Save".

DEFINE BUTTON btnSaveResults 
     IMAGE-UP FILE "AOA/images/navigate_check.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Save Results" 
     SIZE 4.4 BY 1 TOOLTIP "Save Results".

DEFINE BUTTON btnSingle 
     LABEL "'" 
     SIZE 2.4 BY 1.05 TOOLTIP "Single Quote".

DEFINE BUTTON btnStr 
     LABEL "STR (" 
     SIZE 10 BY 1.05 TOOLTIP "String".

DEFINE BUTTON btnSubstr 
     LABEL "SUBSTR(" 
     SIZE 10 BY 1.05 TOOLTIP "Substring".

DEFINE BUTTON btnSyntax 
     IMAGE-UP FILE "AOA/images/navigate_check.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Syntax" 
     SIZE 4.4 BY 1 TOOLTIP "Check Query Syntax".

DEFINE BUTTON btnTime 
     LABEL "TIME" 
     SIZE 10 BY 1.05 TOOLTIP "TIME".

DEFINE BUTTON btnToday 
     LABEL "TODAY" 
     SIZE 10 BY 1.05 TOOLTIP "TODAY".

DEFINE BUTTON btnUpdate 
     IMAGE-UP FILE "Graphics/32x32/pencil.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/pencil_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Update" 
     SIZE 8 BY 1.91 TOOLTIP "Update/Save".

DEFINE BUTTON btnView 
     IMAGE-UP FILE "AOA/images/jrxml_icon.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Jasper Viewer".

DEFINE BUTTON btnWhere 
     LABEL "WHERE" 
     SIZE 10 BY 1.05 TOOLTIP "WHERE".

DEFINE BUTTON btnXLS 
     IMAGE-UP FILE "AOA/images/aoaexcel.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Excel XLS".

DEFINE BUTTON btnYes 
     LABEL "YES" 
     SIZE 5 BY 1.05 TOOLTIP "Yes".

DEFINE VARIABLE cParamDataType AS CHARACTER FORMAT "X(256)":U INITIAL "Character" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEMS "Character","Date","Decimal","Integer","Logical","DateTime" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cParameter AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX SORT INNER-LINES 6
     DROP-DOWN-LIST
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE tableList AS CHARACTER FORMAT "X(256)":U 
     LABEL "Table" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE tableListOf AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE queryStr AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL NO-BOX
     SIZE 76 BY 5.48
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE cConstant AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE cConstantLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Constant:" 
      VIEW-AS TEXT 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE columnSearch AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 64 BY 1 NO-UNDO.

DEFINE VARIABLE cParamDataTypeLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Type:" 
      VIEW-AS TEXT 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE cParameterLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Parameter:" 
      VIEW-AS TEXT 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cParamFormat AS CHARACTER FORMAT "X(256)":U INITIAL "x(8)" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE cParamFormatLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Format:" 
      VIEW-AS TEXT 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE cParamInit AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE cParamInitLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Initial:" 
      VIEW-AS TEXT 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE cParamName AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE cParamNameLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Name:" 
      VIEW-AS TEXT 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE fieldSearch AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE parameterSearch AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE querySearch AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE queryText AS CHARACTER FORMAT "X(256)":U INITIAL " Query" 
      VIEW-AS TEXT 
     SIZE 8 BY .62
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE VARIABLE tableSearch AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE findType AS CHARACTER INITIAL "EACH" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Each", "EACH",
"First", "FIRST",
"Last", "LAST"
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE querySection AS CHARACTER INITIAL "Query" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "&Query", "Query",
"&Table", "Table",
"&Where", "Where",
"S&ort", "Sort",
"&Columns", "Columns",
"&Parameters", "Parameters"
     SIZE 72 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE sortType AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ascending", yes,
"Descending", no
     SIZE 31 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-COLUMN
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 78 BY 1.43.

DEFINE RECTANGLE RECT-FIELD
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 36 BY 1.43.

DEFINE RECTANGLE RECT-JASPER
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 30 BY 1.43.

DEFINE RECTANGLE RECT-PANEL
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 50 BY 2.38
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-PARAMETER
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 36 BY 1.43.

DEFINE RECTANGLE RECT-QUERY
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 37 BY 1.43.

DEFINE RECTANGLE RECT-QUERYSORT
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 47 BY 1.43.

DEFINE RECTANGLE RECT-QUERYSTR
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 83 BY 6.43.

DEFINE RECTANGLE RECT-QUERYTABLE
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 31 BY 1.43.

DEFINE RECTANGLE RECT-SAVE
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 9.8 BY 2.38.

DEFINE RECTANGLE RECT-SECTION
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 74 BY 2.38.

DEFINE RECTANGLE RECT-TABLE
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 36 BY 1.43.

DEFINE VARIABLE columnMatches AS LOGICAL INITIAL yes 
     LABEL "Matches" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE fieldMatches AS LOGICAL INITIAL yes 
     LABEL "Matches" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE parameterMatches AS LOGICAL INITIAL yes 
     LABEL "Matches" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE queryMatches AS LOGICAL INITIAL yes 
     LABEL "Matches" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE tableMatches AS LOGICAL INITIAL no 
     LABEL "Matches" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fieldBrowse FOR 
      ttField SCROLLING.

DEFINE QUERY queryColumnBrowse FOR 
      ttQueryColumn SCROLLING.

DEFINE QUERY queryNameBrowse FOR 
      ttQueryName SCROLLING.

DEFINE QUERY queryParameterBrowse FOR 
      ttQueryParameter SCROLLING.

DEFINE QUERY querySortBrowse FOR 
      ttQuerySort SCROLLING.

DEFINE QUERY queryTableBrowse FOR 
      ttQueryTable SCROLLING.

DEFINE QUERY queryWhereBrowse FOR 
      ttQueryWhere SCROLLING.

DEFINE QUERY tableBrowse FOR 
      ttTable SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE fieldBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS fieldBrowse C-Win _FREEFORM
  QUERY fieldBrowse DISPLAY
      ttField.fieldName LABEL-BGCOLOR 14
ttField.fieldLabel LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 36 BY 4.19
         TITLE "Available Fields".

DEFINE BROWSE queryColumnBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS queryColumnBrowse C-Win _FREEFORM
  QUERY queryColumnBrowse DISPLAY
      ttQueryColumn.fieldName
ttQueryColumn.fieldLabel
ttQueryColumn.fieldFormat
ENABLE
ttQueryColumn.fieldLabel
ttQueryColumn.fieldFormat
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 78 BY 3.81
         TITLE "Query Columns" ROW-HEIGHT-CHARS .62.

DEFINE BROWSE queryNameBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS queryNameBrowse C-Win _FREEFORM
  QUERY queryNameBrowse DISPLAY
      ttQueryName.queryName
ttQueryName.isActive VIEW-AS TOGGLE-BOX
ttQueryName.queryID
ttQueryName.queryType
ttQueryName.module
ttQueryName.user-id
ttQueryName.securityLevel
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 37 BY 4.19
         TITLE "Query" ROW-HEIGHT-CHARS .62.

DEFINE BROWSE queryParameterBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS queryParameterBrowse C-Win _FREEFORM
  QUERY queryParameterBrowse DISPLAY
      ttQueryParameter.paramName
ttQueryParameter.dataType
ttQueryParameter.paramFormat
ttQueryParameter.initValue
ttQueryParameter.paramPrompt VIEW-AS TOGGLE-BOX
ENABLE
ttQueryParameter.paramPrompt
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 120 BY 3.43
         TITLE "Query Parameters".

DEFINE BROWSE querySortBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS querySortBrowse C-Win _FREEFORM
  QUERY querySortBrowse DISPLAY
      ttQuerySort.fieldLabel
ttQuerySort.sortBy
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 78 BY 4.19
         TITLE "Query Sort".

DEFINE BROWSE queryTableBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS queryTableBrowse C-Win _FREEFORM
  QUERY queryTableBrowse DISPLAY
      ttQueryTable.tableFind
ttQueryTable.tableName
ttQueryTable.tableDscr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 78 BY 4.19
         TITLE "Query Tables" ROW-HEIGHT-CHARS .62.

DEFINE BROWSE queryWhereBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS queryWhereBrowse C-Win _FREEFORM
  QUERY queryWhereBrowse DISPLAY
      ttQueryWhere.whereElement
ttQueryWhere.fieldLabel
ttQueryWhere.dataType
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 67 BY 4.19
         TITLE "Query Where".

DEFINE BROWSE tableBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS tableBrowse C-Win _FREEFORM
  QUERY tableBrowse DISPLAY
      ttTable.tableName LABEL-BGCOLOR 14
ttTable.tableDB LABEL-BGCOLOR 14
ttTable.tableDscr LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 36 BY 4.19
         TITLE "Available Tables" ROW-HEIGHT-CHARS .62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnAddParameter AT ROW 20.76 COL 145 WIDGET-ID 208
     querySection AT ROW 2.19 COL 3 HELP
          "Select Query Section" NO-LABEL WIDGET-ID 30
     btnNow AT ROW 2.91 COL 117 WIDGET-ID 194
     btnToday AT ROW 2.91 COL 126 WIDGET-ID 190
     btnTime AT ROW 2.91 COL 136 WIDGET-ID 192
     btnDateTime AT ROW 2.91 COL 146 WIDGET-ID 196
     querySearch AT ROW 3.86 COL 3 NO-LABEL WIDGET-ID 16
     queryMatches AT ROW 3.86 COL 26 HELP
          "Select for Matches Search" WIDGET-ID 38
     tableSearch AT ROW 3.86 COL 41 NO-LABEL WIDGET-ID 2
     tableMatches AT ROW 3.86 COL 64 HELP
          "Select for Table Search Matches" WIDGET-ID 40
     tableList AT ROW 4.1 COL 87 COLON-ALIGNED HELP
          "Select Table" WIDGET-ID 98
     btnOF AT ROW 4.1 COL 112 WIDGET-ID 104
     tableListOf AT ROW 4.1 COL 115 COLON-ALIGNED HELP
          "Select Table" NO-LABEL WIDGET-ID 106
     btnWhere AT ROW 4.1 COL 139 WIDGET-ID 100
     btnMatches AT ROW 4.1 COL 149 WIDGET-ID 88
     queryNameBrowse AT ROW 5.29 COL 2 WIDGET-ID 600
     tableBrowse AT ROW 5.29 COL 40 WIDGET-ID 200
     queryWhereBrowse AT ROW 5.29 COL 82 WIDGET-ID 800
     btnBegins AT ROW 5.29 COL 150 WIDGET-ID 84
     btnAND AT ROW 6.48 COL 150 WIDGET-ID 80
     btnOR AT ROW 6.48 COL 155 WIDGET-ID 82
     btnYes AT ROW 7.67 COL 150 WIDGET-ID 172
     btnNo AT ROW 7.67 COL 155 WIDGET-ID 174
     btnEQ AT ROW 8.86 COL 150 WIDGET-ID 68
     btnNE AT ROW 8.86 COL 155 WIDGET-ID 70
     fieldSearch AT ROW 9.81 COL 39 COLON-ALIGNED HELP
          "Enter Field Search" NO-LABEL WIDGET-ID 50
     fieldMatches AT ROW 9.81 COL 64 HELP
          "Select for Table Search Matches" WIDGET-ID 52
     sortType AT ROW 9.81 COL 91 NO-LABEL WIDGET-ID 22
     btnLT AT ROW 10.05 COL 150 WIDGET-ID 72
     btnGT AT ROW 10.05 COL 155 WIDGET-ID 74
     fieldBrowse AT ROW 11 COL 40 WIDGET-ID 700
     querySortBrowse AT ROW 11 COL 82 WIDGET-ID 500
     btnLE AT ROW 11.24 COL 150 WIDGET-ID 76
     btnGE AT ROW 11.24 COL 155 WIDGET-ID 78
     btnPlus AT ROW 12.43 COL 150 WIDGET-ID 158
     btnMinus AT ROW 12.43 COL 155 WIDGET-ID 156
     btnMultiply AT ROW 13.62 COL 150 WIDGET-ID 160
     btnDivide AT ROW 13.62 COL 155 WIDGET-ID 154
     btnDate AT ROW 14.81 COL 150 WIDGET-ID 166
     columnSearch AT ROW 15.52 COL 70 COLON-ALIGNED HELP
          "Enter Column Search" NO-LABEL WIDGET-ID 112
     columnMatches AT ROW 15.52 COL 137 HELP
          "Select for Column Search Matches" WIDGET-ID 110
     btnDec AT ROW 16 COL 150 WIDGET-ID 164
     queryColumnBrowse AT ROW 16.71 COL 71 WIDGET-ID 900
     btnInt AT ROW 17.19 COL 150 WIDGET-ID 162
     findType AT ROW 17.43 COL 4 NO-LABEL WIDGET-ID 26
     btnStr AT ROW 18.38 COL 150 WIDGET-ID 168
     queryTableBrowse AT ROW 18.62 COL 2 WIDGET-ID 400
     btnSubstr AT ROW 19.57 COL 150 WIDGET-ID 170
     cParameter AT ROW 20.76 COL 91 COLON-ALIGNED HELP
          "Select Parameter Type" NO-LABEL WIDGET-ID 204
     btnOpen AT ROW 20.76 COL 150 WIDGET-ID 94
     btnClose AT ROW 20.76 COL 155 WIDGET-ID 96
     cConstant AT ROW 21.95 COL 91 COLON-ALIGNED NO-LABEL WIDGET-ID 176
     btnPeriod AT ROW 21.95 COL 150 WIDGET-ID 236
     btnDouble AT ROW 21.95 COL 152.4 WIDGET-ID 240
     btnComma AT ROW 21.95 COL 155 WIDGET-ID 242
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     btnSingle AT ROW 21.95 COL 157.4 WIDGET-ID 244
     cParamName AT ROW 23.38 COL 8 COLON-ALIGNED HELP
          "Enter Parameter Name" NO-LABEL WIDGET-ID 218
     queryStr AT ROW 23.86 COL 83 NO-LABEL WIDGET-ID 4
     cParamDataType AT ROW 24.33 COL 8 COLON-ALIGNED HELP
          "Select Parameter Data Type" NO-LABEL WIDGET-ID 222
     parameterSearch AT ROW 24.81 COL 39 COLON-ALIGNED HELP
          "Enter Parameter Search" NO-LABEL WIDGET-ID 216
     parameterMatches AT ROW 24.81 COL 64 HELP
          "Select for Parameter Search Matches" WIDGET-ID 214
     cParamFormat AT ROW 25.29 COL 8 COLON-ALIGNED HELP
          "Enter Parameter Name" NO-LABEL WIDGET-ID 230
     queryParameterBrowse AT ROW 26 COL 40 WIDGET-ID 1000
     cParamInit AT ROW 26.24 COL 8 COLON-ALIGNED HELP
          "Enter Parameter Name" NO-LABEL WIDGET-ID 232
     btnAddParamName AT ROW 24.33 COL 34 WIDGET-ID 234
     btnCloseResults AT ROW 28.38 COL 6 HELP
          "Jasper Viewer" WIDGET-ID 252
     btnResults AT ROW 1.24 COL 131 HELP
          "Jasper Viewer" WIDGET-ID 250
     btnSave AT ROW 16 COL 60 HELP
          "Update/Save" WIDGET-ID 248
     btnSaveResults AT ROW 28.38 COL 2 HELP
          "Jasper Viewer" WIDGET-ID 254
     btnRemoveSelection AT ROW 13.62 COL 77 HELP
          "Remove Selections" WIDGET-ID 198
     btnSyntax AT ROW 23.86 COL 78 WIDGET-ID 202
     btnAddSelections AT ROW 7.43 COL 77 HELP
          "Add Selections" WIDGET-ID 200
     btnMoveDown AT ROW 11.71 COL 77 HELP
          "Move Down" WIDGET-ID 62
     btnAddConstant AT ROW 21.95 COL 145 WIDGET-ID 180
     btnCancel AT ROW 1.24 COL 119 HELP
          "Cancel" WIDGET-ID 120
     btnMoveUp AT ROW 9.33 COL 77 HELP
          "Move Up" WIDGET-ID 64
     btnRemove AT ROW 10.52 COL 77 HELP
          "Remove" WIDGET-ID 66
     btnHTML AT ROW 1.24 COL 151 HELP
          "HTML" WIDGET-ID 144
     btnView AT ROW 1.24 COL 155 HELP
          "Jasper Viewer" WIDGET-ID 148
     btnPDF AT ROW 1.24 COL 147 HELP
          "PDF" WIDGET-ID 146
     btnDOCX AT ROW 1.24 COL 143 HELP
          "Word DOCX" WIDGET-ID 142
     btnCSV AT ROW 1.24 COL 135 HELP
          "Excel CSV" WIDGET-ID 140
     btnXLS AT ROW 1.24 COL 139 HELP
          "Excel XLS" WIDGET-ID 150
     btnAdd AT ROW 1.24 COL 87 HELP
          "Add" WIDGET-ID 118
     btnCopy AT ROW 1.24 COL 95 HELP
          "Copy" WIDGET-ID 122
     btnDelete AT ROW 1.24 COL 103 HELP
          "Delete" WIDGET-ID 124
     btnReset AT ROW 1.24 COL 111 HELP
          "Reset" WIDGET-ID 126
     btnUpdate AT ROW 1.24 COL 79 HELP
          "Update/Save" WIDGET-ID 128
     cParameterLabel AT ROW 20.76 COL 80 COLON-ALIGNED NO-LABEL WIDGET-ID 206
     cConstantLabel AT ROW 21.95 COL 81 COLON-ALIGNED NO-LABEL WIDGET-ID 178
     queryText AT ROW 22.91 COL 78 COLON-ALIGNED NO-LABEL WIDGET-ID 108
     cParamNameLabel AT ROW 23.38 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 220
     cParamDataTypeLabel AT ROW 24.33 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 224
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     cParamFormatLabel AT ROW 25.29 COL 2 NO-LABEL WIDGET-ID 226
     cParamInitLabel AT ROW 26.24 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 228
     "SECTIONS" VIEW-AS TEXT
          SIZE 11 BY .91 AT ROW 1.24 COL 33 WIDGET-ID 36
          FGCOLOR 9 FONT 6
     RECT-SECTION AT ROW 1 COL 2 WIDGET-ID 42
     RECT-QUERY AT ROW 3.62 COL 2 WIDGET-ID 44
     RECT-TABLE AT ROW 3.62 COL 40 WIDGET-ID 46
     RECT-FIELD AT ROW 9.57 COL 40 WIDGET-ID 48
     RECT-QUERYTABLE AT ROW 17.19 COL 2 WIDGET-ID 54
     RECT-QUERYSORT AT ROW 9.57 COL 82 WIDGET-ID 56
     RECT-QUERYSTR AT ROW 23.14 COL 77 WIDGET-ID 58
     RECT-COLUMN AT ROW 15.29 COL 71 WIDGET-ID 114
     RECT-PANEL AT ROW 1 COL 78 WIDGET-ID 130
     RECT-JASPER AT ROW 1 COL 130 WIDGET-ID 152
     RECT-PARAMETER AT ROW 24.57 COL 40 WIDGET-ID 212
     RECT-SAVE AT ROW 15.76 COL 59 WIDGET-ID 246
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Dynamic Query Builder"
         HEIGHT             = 28.57
         WIDTH              = 160
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 384
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB queryNameBrowse btnMatches DEFAULT-FRAME */
/* BROWSE-TAB tableBrowse queryNameBrowse DEFAULT-FRAME */
/* BROWSE-TAB queryWhereBrowse tableBrowse DEFAULT-FRAME */
/* BROWSE-TAB fieldBrowse btnGT DEFAULT-FRAME */
/* BROWSE-TAB querySortBrowse fieldBrowse DEFAULT-FRAME */
/* BROWSE-TAB queryColumnBrowse btnDec DEFAULT-FRAME */
/* BROWSE-TAB queryTableBrowse btnStr DEFAULT-FRAME */
/* BROWSE-TAB queryParameterBrowse cParamFormat DEFAULT-FRAME */
/* SETTINGS FOR BUTTON btnAdd IN FRAME DEFAULT-FRAME
   1 6                                                                  */
/* SETTINGS FOR BUTTON btnAddConstant IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnAddConstant:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnAddParameter IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnAddParameter:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnAddParamName IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       btnAddParamName:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnAddSelections IN FRAME DEFAULT-FRAME
   1 2 3 4 5                                                            */
ASSIGN 
       btnAddSelections:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnAND IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnAND:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnBegins IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnBegins:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnCancel IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 6                                                        */
/* SETTINGS FOR BUTTON btnClose IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnClose:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnCloseResults IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnComma IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnComma:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnCopy IN FRAME DEFAULT-FRAME
   1 6                                                                  */
/* SETTINGS FOR BUTTON btnCSV IN FRAME DEFAULT-FRAME
   1 2 3 4 5 6                                                          */
/* SETTINGS FOR BUTTON btnDate IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnDate:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnDateTime IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnDateTime:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnDec IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnDec:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnDelete IN FRAME DEFAULT-FRAME
   1 6                                                                  */
/* SETTINGS FOR BUTTON btnDivide IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnDivide:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnDOCX IN FRAME DEFAULT-FRAME
   1 2 3 4 5 6                                                          */
/* SETTINGS FOR BUTTON btnDouble IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnDouble:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnEQ IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnEQ:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnGE IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnGE:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnGT IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnGT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnHTML IN FRAME DEFAULT-FRAME
   1 2 3 4 5 6                                                          */
/* SETTINGS FOR BUTTON btnInt IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnInt:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnLE IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnLE:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnLT IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnLT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnMatches IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnMatches:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnMinus IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnMinus:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnMoveDown IN FRAME DEFAULT-FRAME
   1 2 3 4 5                                                            */
ASSIGN 
       btnMoveDown:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnMoveUp IN FRAME DEFAULT-FRAME
   1 2 3 4 5                                                            */
ASSIGN 
       btnMoveUp:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnMultiply IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnMultiply:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnNE IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnNE:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnNo IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnNo:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnNow IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnNow:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnOF IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnOF:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnOpen IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnOpen:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnOR IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnOR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnPDF IN FRAME DEFAULT-FRAME
   1 2 3 4 5 6                                                          */
/* SETTINGS FOR BUTTON btnPeriod IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnPeriod:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnPlus IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnPlus:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnRemove IN FRAME DEFAULT-FRAME
   1 2 3 4 5                                                            */
ASSIGN 
       btnRemove:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnRemoveSelection IN FRAME DEFAULT-FRAME
   1 2 3 4 5                                                            */
ASSIGN 
       btnRemoveSelection:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnReset IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 6                                                        */
/* SETTINGS FOR BUTTON btnResults IN FRAME DEFAULT-FRAME
   1 2 3 4 5 6                                                          */
/* SETTINGS FOR BUTTON btnSave IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnSaveResults IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnSingle IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnSingle:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnStr IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnStr:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnSubstr IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnSubstr:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnSyntax IN FRAME DEFAULT-FRAME
   1 2 3 4                                                              */
ASSIGN 
       btnSyntax:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnTime IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnTime:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnToday IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnToday:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnUpdate IN FRAME DEFAULT-FRAME
   1 6                                                                  */
/* SETTINGS FOR BUTTON btnView IN FRAME DEFAULT-FRAME
   1 2 3 4 5 6                                                          */
/* SETTINGS FOR BUTTON btnWhere IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnWhere:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnXLS IN FRAME DEFAULT-FRAME
   1 2 3 4 5 6                                                          */
/* SETTINGS FOR BUTTON btnYes IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnYes:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN cConstant IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       cConstant:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN cConstantLabel IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       cConstantLabel:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX columnMatches IN FRAME DEFAULT-FRAME
   1 5                                                                  */
ASSIGN 
       columnMatches:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN columnSearch IN FRAME DEFAULT-FRAME
   1 5                                                                  */
ASSIGN 
       columnSearch:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX cParamDataType IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       cParamDataType:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN cParamDataTypeLabel IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       cParamDataTypeLabel:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX cParameter IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       cParameter:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN cParameterLabel IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       cParameterLabel:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN cParamFormat IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       cParamFormat:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN cParamFormatLabel IN FRAME DEFAULT-FRAME
   ALIGN-L 1                                                            */
ASSIGN 
       cParamFormatLabel:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN cParamInit IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       cParamInit:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN cParamInitLabel IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       cParamInitLabel:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN cParamName IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       cParamName:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN cParamNameLabel IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       cParamNameLabel:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BROWSE fieldBrowse IN FRAME DEFAULT-FRAME
   1 3 4 5                                                              */
ASSIGN 
       fieldBrowse:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       fieldBrowse:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR TOGGLE-BOX fieldMatches IN FRAME DEFAULT-FRAME
   1 3 4 5                                                              */
ASSIGN 
       fieldMatches:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN fieldSearch IN FRAME DEFAULT-FRAME
   1 3 4 5                                                              */
ASSIGN 
       fieldSearch:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RADIO-SET findType IN FRAME DEFAULT-FRAME
   1 2                                                                  */
ASSIGN 
       findType:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX parameterMatches IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       parameterMatches:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN parameterSearch IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       parameterSearch:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BROWSE queryColumnBrowse IN FRAME DEFAULT-FRAME
   1 5                                                                  */
ASSIGN 
       queryColumnBrowse:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

/* SETTINGS FOR BROWSE queryParameterBrowse IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       queryParameterBrowse:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

/* SETTINGS FOR FILL-IN querySearch IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR BROWSE querySortBrowse IN FRAME DEFAULT-FRAME
   1 4                                                                  */
ASSIGN 
       querySortBrowse:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

/* SETTINGS FOR EDITOR queryStr IN FRAME DEFAULT-FRAME
   1 2 3 4                                                              */
ASSIGN 
       queryStr:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE
       queryStr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR BROWSE queryTableBrowse IN FRAME DEFAULT-FRAME
   1 2                                                                  */
ASSIGN 
       queryTableBrowse:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

/* SETTINGS FOR FILL-IN queryText IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 2 3 4                                                    */
ASSIGN 
       queryText:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BROWSE queryWhereBrowse IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       queryWhereBrowse:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

/* SETTINGS FOR RECTANGLE RECT-COLUMN IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 5                                                        */
ASSIGN 
       RECT-COLUMN:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-FIELD IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 3 4 5                                                    */
ASSIGN 
       RECT-FIELD:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-JASPER IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 2 3 4 5 6                                                */
/* SETTINGS FOR RECTANGLE RECT-PANEL IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 6                                                        */
/* SETTINGS FOR RECTANGLE RECT-PARAMETER IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
ASSIGN 
       RECT-PARAMETER:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-QUERY IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-QUERYSORT IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 4                                                        */
ASSIGN 
       RECT-QUERYSORT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-QUERYSTR IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 2 3 4                                                    */
ASSIGN 
       RECT-QUERYSTR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-QUERYTABLE IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 2                                                        */
ASSIGN 
       RECT-QUERYTABLE:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-SAVE IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR RECTANGLE RECT-SECTION IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-TABLE IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 2                                                        */
ASSIGN 
       RECT-TABLE:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RADIO-SET sortType IN FRAME DEFAULT-FRAME
   1 4                                                                  */
ASSIGN 
       sortType:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BROWSE tableBrowse IN FRAME DEFAULT-FRAME
   1 2                                                                  */
ASSIGN 
       tableBrowse:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       tableBrowse:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR COMBO-BOX tableList IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       tableList:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX tableListOf IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       tableListOf:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tableMatches IN FRAME DEFAULT-FRAME
   1 2                                                                  */
ASSIGN 
       tableMatches:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN tableSearch IN FRAME DEFAULT-FRAME
   ALIGN-L 1 2                                                          */
ASSIGN 
       tableSearch:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE fieldBrowse
/* Query rebuild information for BROWSE fieldBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttField
WHERE (fieldMatches EQ NO
AND (ttField.fieldName BEGINS fieldSearch
OR ttField.fieldLabel BEGINS fieldSearch))
OR (fieldMatches EQ YES
AND (ttField.fieldName MATCHES "*" + fieldSearch + "*"
OR ttField.fieldLabel MATCHES "*" + fieldSearch + "*"))
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE fieldBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE queryColumnBrowse
/* Query rebuild information for BROWSE queryColumnBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttQueryColumn
WHERE ttQueryColumn.queryID EQ ttQueryName.queryID
  AND ((columnMatches EQ NO  AND ttQueryColumn.fieldName BEGINS columnSearch)
   OR (columnMatches EQ YES AND ttQueryColumn.fieldName MATCHES "*" + columnSearch + "*"))
BY ttQueryColumn.sortOrder.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE queryColumnBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE queryNameBrowse
/* Query rebuild information for BROWSE queryNameBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttQueryName
WHERE (queryMatches EQ NO  AND ttQueryName.queryName BEGINS querySearch)
   OR (queryMatches EQ YES AND ttQueryName.queryName MATCHES "*" + querySearch + "*").
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE queryNameBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE queryParameterBrowse
/* Query rebuild information for BROWSE queryParameterBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttQueryParameter
WHERE ttQueryParameter.queryID EQ ttQueryName.queryID
  AND ((parameterMatches EQ NO  AND ttQueryParameter.paramName BEGINS parameterSearch)
   OR (parameterMatches EQ YES AND ttQueryParameter.paramName MATCHES "*" + parameterSearch + "*")).
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE queryParameterBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE querySortBrowse
/* Query rebuild information for BROWSE querySortBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttQuerySort
WHERE ttQuerySort.queryID EQ ttQueryName.queryID
BY ttQuerySort.sortOrder.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE querySortBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE queryTableBrowse
/* Query rebuild information for BROWSE queryTableBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttQueryTable
WHERE ttQueryTable.queryID EQ ttQueryName.queryID
BY ttQueryTable.sortOrder.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE queryTableBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE queryWhereBrowse
/* Query rebuild information for BROWSE queryWhereBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttQueryWhere
WHERE ttQueryWhere.queryID EQ ttQueryName.queryID
AND ttQueryWhere.whereTable EQ tableList
BY ttQueryWhere.sortOrder.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE queryWhereBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE tableBrowse
/* Query rebuild information for BROWSE tableBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttTable
WHERE (tableMatches EQ NO
AND (ttTable.tableName BEGINS tableSearch
OR ttTable.tableDscr BEGINS tableSearch))
OR (tableMatches EQ YES
AND (ttTable.tableName MATCHES "*" + tableSearch + "*"
OR ttTable.tableDscr MATCHES "*" + tableSearch + "*"))
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE tableBrowse */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Dynamic Query Builder */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Dynamic Query Builder */
DO:
  /* This event will close the window and terminate the procedure.  */
  IF lSave THEN DO WITH FRAME {&FRAME-NAME}:
    MESSAGE
        "SAVE Changes before Exiting?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
    UPDATE lSaveBeforeExit AS LOGICAL.
    IF lSaveBeforeExit THEN
    APPLY "CHOOSE":U TO btnSave.
    ELSE IF lSaveBeforeExit EQ ? THEN
    RETURN NO-APPLY.
  END. /* if lsave */
  RUN pSaveSettings.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Dynamic Query Builder */
DO:
    RUN pWinReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd C-Win
ON CHOOSE OF btnAdd IN FRAME DEFAULT-FRAME /* Add */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddConstant
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddConstant C-Win
ON CHOOSE OF btnAddConstant IN FRAME DEFAULT-FRAME /* Add Constant */
DO:
    ASSIGN cConstant.
    RUN pAddWhere (tableList, cConstant, "").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddParameter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddParameter C-Win
ON CHOOSE OF btnAddParameter IN FRAME DEFAULT-FRAME /* Add Parameter */
DO:
    ASSIGN cParameter.
    IF cParameter NE "" AND cParameter NE ? THEN
    RUN pAddWhere (tableList, "[[" + cParameter + "]]", "").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddParamName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddParamName C-Win
ON CHOOSE OF btnAddParamName IN FRAME DEFAULT-FRAME /* Add Parameter Name */
DO:
    ASSIGN cParamName cParamDataType cParamFormat cParamInit.
    fSetSaveButton (YES).
    RUN pAddParamName.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddSelections
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddSelections C-Win
ON CHOOSE OF btnAddSelections IN FRAME DEFAULT-FRAME /* Add Selection */
DO:
    RUN pAddSelections.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCloseResults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCloseResults C-Win
ON CHOOSE OF btnCloseResults IN FRAME DEFAULT-FRAME /* Close Results */
DO:
    IF VALID-HANDLE(hQueryBrowse) THEN
    DELETE OBJECT hQueryBrowse.
    ASSIGN
        btnSaveResults:HIDDEN = YES
        SELF:HIDDEN           = YES
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCopy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopy C-Win
ON CHOOSE OF btnCopy IN FRAME DEFAULT-FRAME /* Copy */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCSV C-Win
ON CHOOSE OF btnCSV IN FRAME DEFAULT-FRAME /* csv */
DO:
    RUN pRunQuery (YES, "CSV").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete C-Win
ON CHOOSE OF btnDelete IN FRAME DEFAULT-FRAME /* Delete */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDOCX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDOCX C-Win
ON CHOOSE OF btnDOCX IN FRAME DEFAULT-FRAME
DO:
    RUN pRunQuery (YES, "DOCX").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnHTML
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHTML C-Win
ON CHOOSE OF btnHTML IN FRAME DEFAULT-FRAME
DO:
    RUN pRunQuery (YES, "HTML").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveDown C-Win
ON CHOOSE OF btnMoveDown IN FRAME DEFAULT-FRAME
DO:
    RUN pMove (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveUp C-Win
ON CHOOSE OF btnMoveUp IN FRAME DEFAULT-FRAME
DO:
    RUN pMove (-1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOF C-Win
ON CHOOSE OF btnOF IN FRAME DEFAULT-FRAME /* OF */
DO:
    IF tableList NE tableListOf THEN
    RUN pAddWhere (tableList, SELF:LABEL + " " + tableListOf, "").
    ELSE
    MESSAGE
        "Invalid ~"OF~" Condition when both tables are the same."
    VIEW-AS ALERT-BOX ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPDF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPDF C-Win
ON CHOOSE OF btnPDF IN FRAME DEFAULT-FRAME
DO:
    RUN pRunQuery (YES, "PDF").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRemove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemove C-Win
ON CHOOSE OF btnRemove IN FRAME DEFAULT-FRAME
DO:
    CASE querySection:
        WHEN "Columns" THEN
            IF AVAILABLE ttQueryColumn THEN
            APPLY "DEFAULT-ACTION":U TO queryColumnBrowse.
        WHEN "Sort" THEN
            IF AVAILABLE ttQuerySort THEN
            APPLY "DEFAULT-ACTION":U TO querySortBrowse.
        WHEN "Table" THEN
            IF AVAILABLE ttQueryTable THEN
            APPLY "DEFAULT-ACTION":U TO queryTableBrowse.
        WHEN "Where" THEN
            IF AVAILABLE ttQueryWhere THEN
            APPLY "DEFAULT-ACTION":U TO queryWhereBrowse.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRemoveSelection
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemoveSelection C-Win
ON CHOOSE OF btnRemoveSelection IN FRAME DEFAULT-FRAME /* Remove Selection */
DO:
    RUN pRemoveSelections.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset C-Win
ON CHOOSE OF btnReset IN FRAME DEFAULT-FRAME /* Reset */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnResults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnResults C-Win
ON CHOOSE OF btnResults IN FRAME DEFAULT-FRAME /* Results */
DO:
    RUN pRunQuery (YES, "Results").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* Save */
DO:
    fSetSaveButton (NO).
    RUN pSaveQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSaveResults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSaveResults C-Win
ON CHOOSE OF btnSaveResults IN FRAME DEFAULT-FRAME /* Save Results */
DO:
    RUN pSaveResults.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSyntax
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSyntax C-Win
ON CHOOSE OF btnSyntax IN FRAME DEFAULT-FRAME /* Syntax */
DO:
    RUN pRunQuery (NO, ?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUpdate C-Win
ON CHOOSE OF btnUpdate IN FRAME DEFAULT-FRAME /* Update */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnView C-Win
ON CHOOSE OF btnView IN FRAME DEFAULT-FRAME
DO:
    RUN pRunQuery (YES, "View").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnWhere
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnWhere C-Win
ON CHOOSE OF btnWhere IN FRAME DEFAULT-FRAME /* WHERE */
,btnEQ,btnNE,btnLT,btnGT,btnLE,btnGE,btnOpen,btnClose,btnDate,btnDec,btnInt~
,btnStr,btnSubstr,btnYes,btnNo,btnAND,btnOR,btnBegins,btnMatches,btnPlus~
,btnMinus,btnMultiply,btnDivide,btnNow,btnTime,btnToday,btnDateTime~
,btnPeriod,btnDouble,btnComma,btnSingle
DO:
    RUN pAddWhere (tableList, SELF:LABEL, "").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnXLS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnXLS C-Win
ON CHOOSE OF btnXLS IN FRAME DEFAULT-FRAME
DO:
    RUN pRunQuery (YES, "XLS").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cConstant
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cConstant C-Win
ON RETURN OF cConstant IN FRAME DEFAULT-FRAME
DO:
    APPLY "CHOOSE":U TO btnAddConstant.
    APPLY "ENTRY":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME columnMatches
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL columnMatches C-Win
ON VALUE-CHANGED OF columnMatches IN FRAME DEFAULT-FRAME /* Matches */
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-fieldBrowse}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME columnSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL columnSearch C-Win
ON VALUE-CHANGED OF columnSearch IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-queryColumnBrowse}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cParamDataType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cParamDataType C-Win
ON RETURN OF cParamDataType IN FRAME DEFAULT-FRAME
DO:
    APPLY "CHOOSE":U TO btnAddParamName.
    APPLY "ENTRY":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cParamDataType C-Win
ON VALUE-CHANGED OF cParamDataType IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&SELF-NAME}.
    CASE {&SELF-NAME}:
        WHEN "Character" THEN
        cParamFormat:SCREEN-VALUE = "x(8)".
        WHEN "Date" THEN
        cParamFormat:SCREEN-VALUE = "99/99/9999".
        WHEN "Decimal" THEN
        cParamFormat:SCREEN-VALUE = "->>,>>9.99".
        WHEN "Integer" THEN
        cParamFormat:SCREEN-VALUE = "->,>>>,>>9".
        WHEN "Logical" THEN
        cParamFormat:SCREEN-VALUE = "yes/no".
        WHEN "DateTime" THEN
        cParamFormat:SCREEN-VALUE = "99/99/9999 HH:MM:SS.SSS".
    END CASE.
    ASSIGN cParamFormat.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cParameter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cParameter C-Win
ON RETURN OF cParameter IN FRAME DEFAULT-FRAME
DO:
    APPLY "CHOOSE":U TO btnAddParameter.
    APPLY "ENTRY":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cParameter C-Win
ON VALUE-CHANGED OF cParameter IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cParamFormat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cParamFormat C-Win
ON RETURN OF cParamFormat IN FRAME DEFAULT-FRAME
DO:
    APPLY "CHOOSE":U TO btnAddParamName.
    APPLY "ENTRY":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cParamInit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cParamInit C-Win
ON RETURN OF cParamInit IN FRAME DEFAULT-FRAME
DO:
    APPLY "CHOOSE":U TO btnAddParamName.
    APPLY "ENTRY":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cParamName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cParamName C-Win
ON RETURN OF cParamName IN FRAME DEFAULT-FRAME
DO:
    APPLY "CHOOSE":U TO btnAddParamName.
    APPLY "ENTRY":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME fieldBrowse
&Scoped-define SELF-NAME fieldBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fieldBrowse C-Win
ON DEFAULT-ACTION OF fieldBrowse IN FRAME DEFAULT-FRAME /* Available Fields */
DO:
    fSetSaveButton (YES).
    CASE querySection:
        WHEN "Columns" THEN
        RUN pAddColumn (ttField.fieldName).
        WHEN "Sort" THEN
        RUN pAddSort (ttField.fieldName, ttField.fieldLabel).
        WHEN "Table" THEN
        RUN pAddTable (ttTable.tableDB, ttTable.tableName).
        WHEN "Where" THEN
        RUN pAddWhere (tableList, ttField.fieldName, ttField.fieldLabel).
    END CASE.
    fShowQuery().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fieldBrowse C-Win
ON START-SEARCH OF fieldBrowse IN FRAME DEFAULT-FRAME /* Available Fields */
DO:
    IF SELF:CURRENT-COLUMN:NAME NE ? THEN DO:
        cColumnLabel = BROWSE fieldBrowse:CURRENT-COLUMN:NAME.
        IF cColumnLabel EQ cSaveLabel THEN
        lAscending = NOT lAscending.
        cSaveLabel = cColumnLabel.
        RUN pReopenBrowse.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fieldMatches
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fieldMatches C-Win
ON VALUE-CHANGED OF fieldMatches IN FRAME DEFAULT-FRAME /* Matches */
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-fieldBrowse}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fieldSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fieldSearch C-Win
ON VALUE-CHANGED OF fieldSearch IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-fieldBrowse}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME findType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL findType C-Win
ON VALUE-CHANGED OF findType IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&SELF-NAME}.
    IF AVAILABLE ttQueryTable AND ttQueryTable.sortOrder GT 1 THEN DO:
        ttQueryTable.tableFind = {&SELF-NAME}.
        BROWSE queryTableBrowse:REFRESH().
        fShowQuery().
    END. /* if avail */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME parameterMatches
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL parameterMatches C-Win
ON VALUE-CHANGED OF parameterMatches IN FRAME DEFAULT-FRAME /* Matches */
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-fieldBrowse}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME parameterSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL parameterSearch C-Win
ON VALUE-CHANGED OF parameterSearch IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-queryParameterBrowse}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME queryColumnBrowse
&Scoped-define SELF-NAME queryColumnBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL queryColumnBrowse C-Win
ON DEFAULT-ACTION OF queryColumnBrowse IN FRAME DEFAULT-FRAME /* Query Columns */
DO:
    DELETE ttQueryColumn.
    RUN pSetOrder.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME queryMatches
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL queryMatches C-Win
ON VALUE-CHANGED OF queryMatches IN FRAME DEFAULT-FRAME /* Matches */
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-queryNameBrowse}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME queryNameBrowse
&Scoped-define SELF-NAME queryNameBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL queryNameBrowse C-Win
ON VALUE-CHANGED OF queryNameBrowse IN FRAME DEFAULT-FRAME /* Query */
DO:
    {&OPEN-QUERY-queryTableBrowse}
    IF AVAILABLE ttQueryTable THEN
    APPLY "VALUE-CHANGED":U TO BROWSE queryTableBrowse.
    {&OPEN-QUERY-querySortBrowse}
    IF AVAILABLE ttQuerySort THEN
    APPLY "VALUE-CHANGED":U TO BROWSE querySortBrowse.
    {&OPEN-QUERY-queryColumnBrowse}
    RUN pGetFields.
    {&OPEN-QUERY-queryParameterBrowse}
    RUN pGetParamList.
    APPLY "VALUE-CHANGED":U TO tableList.
    fShowQuery().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME queryParameterBrowse
&Scoped-define SELF-NAME queryParameterBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL queryParameterBrowse C-Win
ON DEFAULT-ACTION OF queryParameterBrowse IN FRAME DEFAULT-FRAME /* Query Parameters */
DO:
    fSetSaveButton (YES).
    cLabel = "[[" + ttQueryParameter.paramName + "]]".
    IF CAN-FIND(FIRST ttQueryWhere
                WHERE ttQueryWhere.queryID      EQ ttQueryName.queryID
                  AND ttQueryWhere.whereElement EQ cLabel) THEN DO:
        FOR EACH ttQueryWhere
            WHERE ttQueryWhere.queryID      EQ ttQueryName.queryID
              AND ttQueryWhere.whereElement EQ cLabel
            :
            DELETE ttQueryWhere.
        END. /* each ttquerywhere */
        RUN pSetOrder.
        {&OPEN-QUERY-queryWhereBrowse}
    END. /* if can-find */
    DELETE ttQueryParameter.
    RUN pGetParamList.
    {&OPEN-QUERY-queryParameterBrowse}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME querySearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL querySearch C-Win
ON VALUE-CHANGED OF querySearch IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-queryNameBrowse}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME querySection
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL querySection C-Win
ON VALUE-CHANGED OF querySection IN FRAME DEFAULT-FRAME
DO:
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    HIDE {&allSection}.
    ASSIGN 
        {&SELF-NAME}
        RECT-SAVE:HIDDEN = NOT lSave
        btnSave:HIDDEN   = NOT lSave
        .
    CASE {&SELF-NAME}:
        WHEN "Columns" THEN
        VIEW {&columnsSection}.
        WHEN "Parameters" THEN
        VIEW {&parameterSection}.
        WHEN "Query" THEN
        VIEW {&querySection}.
        WHEN "Sort" THEN
        VIEW {&sortSection}.
        WHEN "Table" THEN
        VIEW {&tableSection}.
        WHEN "Where" THEN
        VIEW {&whereSection}.
    END CASE.
    BROWSE queryNameBrowse:WIDTH = IF {&SELF-NAME} NE "Query" THEN 37
        ELSE FRAME {&FRAME-NAME}:WIDTH - BROWSE queryNameBrowse:COL.
    RUN LockWindowUpdate (0,OUTPUT i).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME querySortBrowse
&Scoped-define SELF-NAME querySortBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL querySortBrowse C-Win
ON DEFAULT-ACTION OF querySortBrowse IN FRAME DEFAULT-FRAME /* Query Sort */
DO:
    DELETE ttQuerySort.
    RUN pSetOrder.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL querySortBrowse C-Win
ON VALUE-CHANGED OF querySortBrowse IN FRAME DEFAULT-FRAME /* Query Sort */
DO:
    ASSIGN
        sortType:SCREEN-VALUE = STRING(ttQuerySort.sortAsc)
        sortType
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME queryTableBrowse
&Scoped-define SELF-NAME queryTableBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL queryTableBrowse C-Win
ON DEFAULT-ACTION OF queryTableBrowse IN FRAME DEFAULT-FRAME /* Query Tables */
DO:
    RUN pDeleteSections.
    DELETE ttQueryTable.
    RUN pGetFields.
    RUN pSetOrder.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL queryTableBrowse C-Win
ON VALUE-CHANGED OF queryTableBrowse IN FRAME DEFAULT-FRAME /* Query Tables */
DO:
    ASSIGN
        findType:SCREEN-VALUE = STRING(ttQueryTable.tableFind)
        findType
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME queryWhereBrowse
&Scoped-define SELF-NAME queryWhereBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL queryWhereBrowse C-Win
ON DEFAULT-ACTION OF queryWhereBrowse IN FRAME DEFAULT-FRAME /* Query Where */
DO:
    DELETE ttQueryWhere.
    RUN pSetOrder.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sortType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sortType C-Win
ON VALUE-CHANGED OF sortType IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&SELF-NAME}.
    IF AVAILABLE ttQuerySort THEN DO:
        ASSIGN
            ttQuerySort.sortAsc = {&SELF-NAME}
            ttQuerySort.sortBy  = fSortBy()
            .
        BROWSE querySortBrowse:REFRESH().
        fShowQuery().
    END. /* if avail */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME tableBrowse
&Scoped-define SELF-NAME tableBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tableBrowse C-Win
ON DEFAULT-ACTION OF tableBrowse IN FRAME DEFAULT-FRAME /* Available Tables */
DO:    
    fSetSaveButton (YES).
    RUN pAddTable (ttTable.tableDB, ttTable.tableName, ttTable.tableDscr).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tableBrowse C-Win
ON START-SEARCH OF tableBrowse IN FRAME DEFAULT-FRAME /* Available Tables */
DO:
    IF SELF:CURRENT-COLUMN:NAME NE ? THEN DO:
        cColumnLabel = BROWSE tableBrowse:CURRENT-COLUMN:NAME.
        IF cColumnLabel EQ cSaveLabel THEN
        lAscending = NOT lAscending.
        cSaveLabel = cColumnLabel.
        RUN pReopenBrowse.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tableList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tableList C-Win
ON VALUE-CHANGED OF tableList IN FRAME DEFAULT-FRAME /* Table */
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-queryWhereBrowse}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tableListOf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tableListOf C-Win
ON VALUE-CHANGED OF tableListOf IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tableMatches
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tableMatches C-Win
ON VALUE-CHANGED OF tableMatches IN FRAME DEFAULT-FRAME /* Matches */
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-tableBrowse}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tableSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tableSearch C-Win
ON VALUE-CHANGED OF tableSearch IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-tableBrowse}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME fieldBrowse
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.


&IF DEFINED(UIB_is_Running) EQ 0 &THEN
RUN util/CheckModule.p ("ASI","QueryBuilder", YES, OUTPUT lContinue).
&ELSE
lContinue = YES.
&ENDIF

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  IF lContinue THEN DO:
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    RUN pLoadQuery.
    RUN pSetObjects.
    RUN pGetSettings.
    RUN enable_UI.
    IF AVAILABLE ttQueryName THEN DO:
        RUN pGetDBTables.
        APPLY "VALUE-CHANGED":U TO BROWSE queryNameBrowse.
        RUN pGetParamList.
    END. /* if avail */
    RUN LockWindowUpdate (0,OUTPUT i).
  END. /* if continue */
  APPLY "VALUE-CHANGED":U TO querySection.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
  IF NOT lContinue THEN
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

&Scoped-define sdBrowseName tableBrowse
{methods/sortByProc.i "pByTableName" "ttTable.tableName"}
{methods/sortByProc.i "pByTableDB" "ttTable.tableDB"}
{methods/sortByProc.i "pByTableDscr" "ttTable.tableDscr"}

&Scoped-define sdBrowseName fieldBrowse
{methods/sortByProc.i "pByFieldName" "ttField.fieldName"}
{methods/sortByProc.i "pByFieldLabel" "ttField.fieldLabel"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY querySection querySearch queryMatches tableSearch tableMatches 
          tableList tableListOf fieldSearch fieldMatches sortType columnSearch 
          columnMatches findType cParameter cConstant cParamName queryStr 
          cParamDataType parameterSearch parameterMatches cParamFormat 
          cParamInit cParameterLabel cConstantLabel queryText cParamNameLabel 
          cParamDataTypeLabel cParamFormatLabel cParamInitLabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnAddParameter querySection btnNow btnToday btnTime btnDateTime 
         querySearch queryMatches tableSearch tableMatches tableList btnOF 
         tableListOf btnWhere btnMatches queryNameBrowse tableBrowse 
         queryWhereBrowse btnBegins btnAND btnOR btnYes btnNo btnEQ btnNE 
         fieldSearch fieldMatches sortType btnLT btnGT fieldBrowse 
         querySortBrowse btnLE btnGE btnPlus btnMinus btnMultiply btnDivide 
         btnDate columnSearch columnMatches btnDec queryColumnBrowse btnInt 
         findType btnStr queryTableBrowse btnSubstr cParameter btnOpen btnClose 
         cConstant btnPeriod btnDouble btnComma btnSingle cParamName queryStr 
         cParamDataType parameterSearch parameterMatches cParamFormat 
         queryParameterBrowse cParamInit btnAddParamName btnCloseResults 
         btnResults btnSave btnSaveResults btnRemoveSelection btnSyntax 
         btnAddSelections btnMoveDown btnAddConstant btnMoveUp btnRemove 
         btnHTML btnView btnPDF btnDOCX btnCSV btnXLS btnAdd btnCopy btnDelete 
         btnUpdate cParameterLabel cConstantLabel cParamNameLabel 
         cParamDataTypeLabel cParamFormatLabel cParamInitLabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddColumn C-Win 
PROCEDURE pAddColumn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFieldName  AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cTable AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cField AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExt   AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bttQueryColumn FOR ttQueryColumn.

    IF CAN-FIND(FIRST ttQueryColumn
                WHERE ttQueryColumn.queryID   EQ ttQueryName.queryID
                  AND ttQueryColumn.fieldName EQ ipcFieldName) THEN
    RETURN.
    CREATE ALIAS "dictdb" FOR DATABASE VALUE(ttField.tableDB).
    ASSIGN
        cTable = ENTRY(1,ipcFieldName,".")
        cField = ENTRY(2,ipcFieldName,".")
        .
    IF INDEX(cField,"[") NE 0 THEN
    ASSIGN
        cExt   = SUBSTR(cField,INDEX(cField,"["))
        cField = SUBSTR(cField,1,INDEX(cField,"[") - 1)
        .
    RUN nosweat/get_frmt.p (
        cTable,
        cField,
        OUTPUT cFormat
        ).
    RUN nosweat/fld_lbls.p (
        cTable,
        cField,
        OUTPUT cLabel
        ).
    IF cExt NE "" THEN
    cLabel = cLabel + cExt.
    FOR EACH bttQueryColumn NO-LOCK
        WHERE bttQueryColumn.queryID EQ ttQueryName.queryID
           BY bttQueryColumn.sortOrder DESCENDING
        :
        LEAVE.
    END. /* for each */
    iOrder = IF AVAILABLE bttQueryColumn THEN bttQueryColumn.sortOrder ELSE 0.
    CREATE ttQueryColumn.
    ASSIGN
        ttQueryColumn.queryID     = ttQueryName.queryID
        ttQueryColumn.sortOrder   = iOrder + 1
        ttQueryColumn.fieldName   = ipcFieldName
        ttQueryColumn.fieldLabel  = cLabel
        ttQueryColumn.fieldFormat = cFormat
        ttQueryColumn.tableName   = cTable
        ttQueryColumn.tableDB     = ttField.tableDB
        rRowID                    = ROWID(ttQueryColumn)
        .
    {&OPEN-QUERY-queryColumnBrowse}
    REPOSITION queryColumnBrowse TO ROWID rRowID.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddParamName C-Win 
PROCEDURE pAddParamName :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF cParamName EQ "" THEN RETURN.
    IF CAN-FIND(FIRST ttQueryParameter
                WHERE ttQueryParameter.queryID   EQ ttQueryName.queryID
                  AND ttQueryParameter.paramName EQ cParamName) THEN
    RETURN.
    CREATE ttQueryParameter.
    ASSIGN
        ttQueryParameter.queryID     = ttQueryName.queryID
        ttQueryParameter.paramName   = cParamName
        ttQueryParameter.dataType    = cParamDataType
        ttQueryParameter.paramFormat = cParamFormat
        ttQueryParameter.initValue   = cParamInit
        .
    RUN pGetParamList.
    {&OPEN-QUERY-queryParameterBrowse}
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddSelections C-Win 
PROCEDURE pAddSelections :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        CASE querySection:
            WHEN "Table" THEN DO:
                DO idx = 1 TO tableBrowse:NUM-SELECTED-ROWS:
                    tableBrowse:FETCH-SELECTED-ROW(idx).
                    RUN pAddTable (ttTable.tableDB, ttTable.TableName, ttTable.tableDscr).
                END. /* do idex */
            END. /* table */
            OTHERWISE DO:
                DO idx = 1 TO fieldBrowse:NUM-SELECTED-ROWS:
                    fieldBrowse:FETCH-SELECTED-ROW(idx).
                    CASE querySection:
                        WHEN "Columns" THEN
                        RUN pAddColumn (ttField.fieldName).
                        WHEN "Sort" THEN
                        RUN pAddSort (ttField.fieldName, ttField.fieldLabel).
                        WHEN "Where" THEN
                        RUN pAddWhere (tableList, ttField.fieldName, ttField.fieldLabel).
                    END CASE.
                END. /* do idex */
            END. /* otherwise */
        END CASE.
    END. /* do with */
    fSetSaveButton (YES).
    fShowQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddSort C-Win 
PROCEDURE pAddSort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFieldName  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFieldLabel AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bttQuerySort FOR ttQuerySort.

    IF CAN-FIND(FIRST ttQuerySort
                WHERE ttQuerySort.queryID   EQ ttQueryName.queryID
                  AND ttQuerySort.tableName EQ ENTRY(1,ipcFieldName,".")
                  AND ttQuerySort.fieldName EQ ENTRY(2,ipcFieldName,".")) THEN
    RETURN.
    FOR EACH bttQuerySort NO-LOCK
        WHERE bttQuerySort.queryID EQ ttQueryName.queryID
           BY bttQuerySort.sortOrder DESCENDING
        :
        LEAVE.
    END. /* for each */
    iOrder = IF AVAILABLE ttQuerySort THEN ttQuerySort.sortOrder ELSE 0.
    CREATE ttQuerySort.
    ASSIGN
        ttQuerySort.queryID    = ttQueryName.queryID
        ttQuerySort.sortOrder  = iOrder + 1
        ttQuerySort.tableName  = ENTRY(1,ipcFieldName,".")
        ttQuerySort.fieldName  = ENTRY(2,ipcFieldName,".")
        ttQuerySort.fieldLabel = ipcFieldLabel
        ttQuerySort.sortAsc    = sortType
        ttQuerySort.sortBy     = fSortBy()
        rRowID                 = ROWID(ttQuerySort)
        .
    {&OPEN-QUERY-querySortBrowse}
    REPOSITION querySortBrowse TO ROWID rRowID.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddTable C-Win 
PROCEDURE pAddTable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTableDB   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTableName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTableDscr AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bttQueryTable FOR ttQueryTable.

    IF CAN-FIND(FIRST ttQueryTable
                WHERE ttQueryTable.queryID   EQ ttQueryName.queryID
                  AND ttQueryTable.tableName EQ ipcTableName) THEN
    RETURN NO-APPLY.
    FOR EACH bttQueryTable NO-LOCK
        WHERE bttQueryTable.queryID EQ ttQueryName.queryID
           BY bttQueryTable.sortOrder DESCENDING
        :
        LEAVE.
    END. /* for each */
    iOrder = IF AVAILABLE bttQueryTable THEN bttQueryTable.sortOrder ELSE 0.
    CREATE ttQueryTable.
    ASSIGN
        ttQueryTable.queryID   = ttQueryName.queryID
        ttQueryTable.sortOrder = iOrder + 1
        ttQueryTable.tableName = ipcTableName
        ttQueryTable.tableDscr = ipcTableDscr
        ttQueryTable.tableFind = findType
        ttQueryTable.tableDB   = ipcTableDB
        rRowID                 = ROWID(ttQueryTable)
        .
    {&OPEN-QUERY-queryTableBrowse}
    REPOSITION queryTableBrowse TO ROWID rRowID.
    RUN pGetFields.
    fShowQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddWhere C-Win 
PROCEDURE pAddWhere :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTableName  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcElement    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTableLabel AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cTable AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cField AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bttQueryWhere FOR ttQueryWhere.
    
    ASSIGN
        cTable    = ipcTableName
        cDataType = ""
        .
    IF INDEX(ipcElement,".") NE 0 THEN DO:
        ASSIGN
            cTable    = ENTRY(1,ipcElement,".")
            cField    = ENTRY(2,ipcElement,".")        
            .
        IF INDEX(cField,"[") NE 0 THEN
        cField = SUBSTR(cField,1,INDEX(cField,"[") - 1).
        RUN nosweat/get_type.p (
            cTable,
            cField,
            OUTPUT cDataType
            ).
        cDataType = REPLACE(cDataType,"STRING","character").
    END. /* if index */
    FOR EACH bttQueryWhere NO-LOCK
        WHERE bttQueryWhere.queryID    EQ ttQueryName.queryID
          AND bttQueryWhere.whereTable EQ tableList
           BY bttQueryWhere.sortOrder DESCENDING
        :
        LEAVE.
    END. /* for each */
    iOrder = IF AVAILABLE bttQueryWhere THEN bttQueryWhere.sortOrder ELSE 0.
    CREATE ttQueryWhere.
    ASSIGN
        ttQueryWhere.queryID      = ttQueryName.queryID
        ttQueryWhere.sortOrder    = iOrder + 1
        ttQueryWhere.tableName    = cTable
        ttQueryWhere.whereTable   = ipcTableName
        ttQueryWhere.whereElement = ipcElement
        ttQueryWhere.fieldLabel   = ipcTableLabel
        ttQueryWhere.dataType     = cDataType
        rRowID                    = ROWID(ttQueryWhere)
        .
    {&OPEN-QUERY-queryWhereBrowse}
    REPOSITION queryWhereBrowse TO ROWID rRowID.
    fShowQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCRUD C-Win 
PROCEDURE pCRUD :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphMode AS HANDLE NO-UNDO.
    
    fSetSaveButton (NO).
    fShowQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDeleteSections C-Win 
PROCEDURE pDeleteSections :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF CAN-FIND(FIRST ttQueryColumn
            WHERE ttQueryColumn.queryID   EQ ttQueryName.queryID
              AND ttQueryColumn.tableName EQ ttQueryTable.tableName) THEN DO:
        FOR EACH ttQueryColumn
            WHERE ttQueryColumn.queryID   EQ ttQueryName.queryID
              AND ttQueryColumn.tableName EQ ttQueryTable.tableName
            :
            DELETE ttQueryColumn.
        END. /* each ttquerysort */
        querySection = "Column".
        RUN pSetOrder.
    END. /* if can-find */
    IF CAN-FIND(FIRST ttQuerySort
            WHERE ttQuerySort.queryID   EQ ttQueryName.queryID
              AND ttQuerySort.tableName EQ ttQueryTable.tableName) THEN DO:
        FOR EACH ttQuerySort
            WHERE ttQuerySort.queryID   EQ ttQueryName.queryID
              AND ttQuerySort.tableName EQ ttQueryTable.tableName
            :
            DELETE ttQuerySort.
        END. /* each ttquerysort */
        querySection = "Sort".
        RUN pSetOrder.
    END. /* if can-find */
    IF CAN-FIND(FIRST ttQueryWhere
            WHERE ttQueryWhere.queryID   EQ ttQueryName.queryID
              AND ttQueryWhere.tableName EQ ttQueryTable.tableName) THEN DO:
        FOR EACH ttQueryWhere
            WHERE ttQueryWhere.queryID   EQ ttQueryName.queryID
              AND ttQueryWhere.tableName EQ ttQueryTable.tableName
            :
            DELETE ttQueryWhere.
        END. /* each ttquerywhere */
        querySection = "Where".
        RUN pSetOrder.
    END. /* if can-find */
    querySection = "Table".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFieldList C-Win 
PROCEDURE pFieldList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcDBName    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTableName AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cFieldList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.

    CREATE ALIAS "dictdb" FOR DATABASE VALUE(ipcDBName).
    RUN nosweat/fld_list.p (ipcTableName, OUTPUT cFieldList).
    DO idx = 1 TO NUM-ENTRIES(cFieldList):
        CREATE ttField.
        ttField.fieldName = ipcTableName + "."
                          + ENTRY(idx,cFieldList)
                          .
    END. /* do idx */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetDBTables C-Win 
PROCEDURE pGetDBTables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDBList    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTableDscr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTableName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx        AS INTEGER   NO-UNDO.
    
    EMPTY TEMP-TABLE ttTable.
    RUN nosweat/db_list.p (OUTPUT cDBList).
    DO idx = 1 TO NUM-ENTRIES(cDBList):
        CREATE ALIAS "dictdb" FOR DATABASE VALUE(ENTRY(idx,cDBList)).
        RUN nosweat/tables.p (OUTPUT cTableName, OUTPUT cTableDscr).
        DO jdx = 1 TO NUM-ENTRIES(cTableName):
            CREATE ttTable.
            ASSIGN
                ttTable.tableDB   = ENTRY(idx,cDBList)
                ttTable.tableName = ENTRY(jdx,cTableName)
                ttTable.tableDscr = ENTRY(jdx,cTableDscr)
                .
        END. /* do jdx */
    END. /* do idx */
    {&OPEN-QUERY-tableBrowse}
    FIND FIRST ttTable.
    APPLY "VALUE-CHANGED":U TO BROWSE tableBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetFields C-Win 
PROCEDURE pGetFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cField     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLabel     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.

    DEFINE BUFFER ttQueryTable FOR ttQueryTable.
    
    EMPTY TEMP-TABLE ttField.
    ASSIGN
        tableList:LIST-ITEMS IN FRAME {&FRAME-NAME} = ?
        tableListOf:LIST-ITEMS = ?
        .
    FOR EACH ttQueryTable
        WHERE ttQueryTable.queryID EQ ttQueryName.queryID
        :
        tableList:ADD-LAST(ttQueryTable.tableName).
        tableListOf:ADD-LAST(ttQueryTable.tableName).
        CREATE ALIAS "dictdb" FOR DATABASE VALUE(ttQueryTable.tableDB).
        RUN nosweat/fld_list.p (ttQueryTable.tableName, OUTPUT cFieldList).
        DO idx = 1 TO NUM-ENTRIES(cFieldList):
            cField = ENTRY(idx,cFieldList).
            IF INDEX(cField,"[") NE 0 THEN
            cField = SUBSTR(cField,1,INDEX(cField,"[") - 1).
            RUN nosweat/fld_lbls.p (ttQueryTable.tableName, cField, OUTPUT cLabel).
            IF cLabel EQ "" THEN
            cLabel = ENTRY(idx,cFieldList).
            CREATE ttField.
            ASSIGN
                ttField.tableDB    = ttQueryTable.tableDB
                ttField.fieldLabel = cLabel
                ttField.fieldName  = ttQueryTable.tableName + "."
                                   + ENTRY(idx,cFieldList)
                                   .
        END. /* do idx */
    END. /* each ttquerytable */
    IF tableList:NUM-ITEMS GT 0 THEN
    ASSIGN
        tableList:SCREEN-VALUE = tableList:ENTRY(1)
        tableListOf:SCREEN-VALUE = tableListOf:ENTRY(1)
        tableList
        tableListOf
        .
    {&OPEN-QUERY-fieldBrowse}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetParamList C-Win 
PROCEDURE pGetParamList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    cParameter:LIST-ITEMS IN FRAME {&FRAME-NAME} = ?.
    FOR EACH ttQueryParameter
        WHERE ttQueryParameter.queryID EQ ttQueryName.queryID
        :
        cParameter:ADD-LAST(ttQueryParameter.paramName).
    END. /* each ttqueryparameter */
    IF cParameter:NUM-ITEMS GT 0 THEN
    ASSIGN
        cParameter:SCREEN-VALUE = cParameter:ENTRY(1)
        cParameter
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSettings C-Win 
PROCEDURE pGetSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    FIND FIRST user-print NO-LOCK
         WHERE user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ USERID("ASI")
         NO-ERROR.
    IF AVAILABLE user-print THEN DO:
        DO idx = 1 TO EXTENT(user-print.field-name):
            IF user-print.field-name[idx] EQ "" THEN LEAVE.
            CASE user-print.field-name[idx]:
                WHEN "WindowColumn" THEN
                {&WINDOW-NAME}:COLUMN = DECIMAL(user-print.field-value[idx]).
                WHEN "WindowRow" THEN
                {&WINDOW-NAME}:ROW = DECIMAL(user-print.field-value[idx]).
                WHEN "WindowWidth" THEN
                ASSIGN
                    {&WINDOW-NAME}:WIDTH = DECIMAL(user-print.field-value[idx])
                    FRAME {&FRAME-NAME}:VIRTUAL-WIDTH = {&WINDOW-NAME}:WIDTH
                    .
                WHEN "WindowHeight" THEN
                ASSIGN
                    {&WINDOW-NAME}:HEIGHT = DECIMAL(user-print.field-value[idx])
                    FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
                    .
            END CASE.
        END. /* do idx */
    END. /* if avail */
    RUN pWinReSize.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLoadQuery C-Win 
PROCEDURE pLoadQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bttQueryName      FOR ttQueryName.
    DEFINE BUFFER bttQueryTable     FOR ttQueryTable.
    DEFINE BUFFER bttQueryWhere     FOR ttQueryWhere.
    DEFINE BUFFER bttQuerySort      FOR ttQuerySort.
    DEFINE BUFFER bttQueryColumn    FOR ttQueryColumn.
    DEFINE BUFFER bttQueryParameter FOR ttQueryParameter.
    
    INPUT FROM VALUE("users/" + USERID("ASI") + "/Query/ttQueryName.d").
    REPEAT:
        CREATE bttQueryName.
        IMPORT bttQueryName.
    END.
    INPUT CLOSE.
    DELETE bttQueryName.
    INPUT FROM VALUE("users/" + USERID("ASI") + "/Query/ttQueryTable.d").
    REPEAT:
        CREATE bttQueryTable.
        IMPORT bttQueryTable.
    END.
    INPUT CLOSE.
    DELETE bttQueryTable.
    INPUT FROM VALUE("users/" + USERID("ASI") + "/Query/ttQueryWhere.d").
    REPEAT:
        CREATE bttQueryWhere.
        IMPORT bttQueryWhere.
    END.
    INPUT CLOSE.
    DELETE bttQueryWhere.
    INPUT FROM VALUE("users/" + USERID("ASI") + "/Query/ttQuerySort.d").
    REPEAT:
        CREATE bttQuerySort.
        IMPORT bttQuerySort.
    END.
    INPUT CLOSE.
    DELETE bttQuerySort.
    INPUT FROM VALUE("users/" + USERID("ASI") + "/Query/ttQueryColumn.d").
    REPEAT:
        CREATE bttQueryColumn.
        IMPORT bttQueryColumn.
    END.
    INPUT CLOSE.
    DELETE bttQueryColumn.
    INPUT FROM VALUE("users/" + USERID("ASI") + "/Query/ttQueryParameter.d").
    REPEAT:
        CREATE bttQueryParameter.
        IMPORT bttQueryParameter.
    END.
    INPUT CLOSE.
    DELETE bttQueryParameter.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMove C-Win 
PROCEDURE pMove :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiMove AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE iCurrent AS INTEGER NO-UNDO.
    DEFINE VARIABLE iMoveTo  AS INTEGER NO-UNDO.
    DEFINE VARIABLE rRowID   AS ROWID   NO-UNDO.
    
    DEFINE BUFFER bttQueryColumn FOR ttQueryColumn.
    DEFINE BUFFER bttQuerySort   FOR ttQuerySort.
    DEFINE BUFFER bttQueryTable  FOR ttQueryTable.
    DEFINE BUFFER bttQueryWhere  FOR ttQueryWhere.
    
    CASE querySection:
        WHEN "Columns" THEN DO:
            {AOA/includes/pMove.i "ttQueryColumn" "queryColumnBrowse"}
        END. /* columns */
        WHEN "Sort" THEN DO:
            {AOA/includes/pMove.i "ttQuerySort" "querySortBrowse"}
        END. /* sort */
        WHEN "Table" THEN DO:
            {AOA/includes/pMove.i "ttQueryTable" "queryTableBrowse"}
        END. /* table */
        WHEN "Where" THEN DO:
            {AOA/includes/pMove.i "ttQueryWhere" "queryWhereBrowse"}
        END. /* where */
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRemoveSelections C-Win 
PROCEDURE pRemoveSelections :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        CASE querySection:
            WHEN "Columns" THEN DO:
                DO idx = 1 TO queryColumnBrowse:NUM-SELECTED-ROWS:
                    queryColumnBrowse:FETCH-SELECTED-ROW(idx).
                    DELETE ttQueryColumn.
                END. /* do idex */
            END. /* columns */
            WHEN "Sort" THEN DO:
                DO idx = 1 TO querySortBrowse:NUM-SELECTED-ROWS:
                    querySortBrowse:FETCH-SELECTED-ROW(idx).
                    DELETE ttQuerySort.
                END. /* do idex */
            END. /* sort */
            WHEN "Table" THEN DO:
                DO idx = 1 TO queryTableBrowse:NUM-SELECTED-ROWS:
                    queryTableBrowse:FETCH-SELECTED-ROW(idx).
                    RUN pDeleteSections.
                    DELETE ttQueryTable.
                END. /* do idex */
                RUN pGetFields.
            END. /* table */
            WHEN "Where" THEN DO:
                DO idx = 1 TO queryWhereBrowse:NUM-SELECTED-ROWS:
                    queryWhereBrowse:FETCH-SELECTED-ROW(idx).
                    DELETE ttQueryWhere.
                END. /* do idex */
            END. /* where */
        END CASE.
    END. /* do with */
    RUN pSetOrder.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReopenBrowse C-Win 
PROCEDURE pReopenBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    SESSION:SET-WAIT-STATE("General").
    CASE cColumnLabel:
        WHEN "fieldLabel" THEN
        RUN pByFieldLabel.
        WHEN "fieldName" THEN
        RUN pByFieldName.
        WHEN "tableDB" THEN
        RUN pByTableDB.
        WHEN "tableDscr" THEN
        RUN pByTableDscr.
        WHEN "tableName" THEN
        RUN pByTableName.
    END CASE.
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pResultsBrowser C-Win 
PROCEDURE pResultsBrowser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphQuery AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE hColumn AS HANDLE NO-UNDO.
    
    DEFINE BUFFER bttQueryColumn FOR ttQueryColumn.
    
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    IF VALID-HANDLE(hQueryBrowse) THEN
    DELETE OBJECT hQueryBrowse.
    
    iphQuery:QUERY-OPEN.
    CREATE BROWSE hQueryBrowse
        ASSIGN
            FRAME = FRAME {&FRAME-NAME}:HANDLE
            TITLE = "Query Results"
            SENSITIVE = TRUE
            SEPARATORS = TRUE
            ROW-MARKERS = FALSE
            COLUMN-RESIZABLE = TRUE
            COLUMN-MOVABLE = TRUE 
            ALLOW-COLUMN-SEARCHING = TRUE
            QUERY = iphQuery
            COL = 1
            ROW = 1
            WIDTH = FRAME {&FRAME-NAME}:WIDTH
            HEIGHT = FRAME {&FRAME-NAME}:HEIGHT
            VISIBLE = TRUE
            NO-VALIDATE = TRUE
            .
    FOR EACH bttQueryColumn
        WHERE bttQueryColumn.queryID EQ ttQueryName.queryID
           BY bttQueryColumn.sortOrder
        :
        hColumn = hQueryBrowse:ADD-LIKE-COLUMN(bttQueryColumn.fieldName).
        IF bttQueryColumn.sortOrder MOD 2 EQ 0 THEN
        hColumn:COLUMN-BGCOLOR = 11.
        IF bttQueryColumn.columnSize NE 0 THEN
        hColumn:WIDTH-CHARS = bttQueryColumn.columnSize.
    END. /* each ttquerycolumn */
    ASSIGN
        btnCloseResults:HIDDEN = NO
        btnSaveResults:HIDDEN  = NO
        .
    btnCloseResults:MOVE-TO-TOP().
    btnSaveResults:MOVE-TO-TOP().
    RUN LockWindowUpdate (0,OUTPUT i).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pResultsJasper C-Win 
PROCEDURE pResultsJasper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphQuery AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER ipcType  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cBufferValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJasperFile  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTableName   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hQueryBuf    AS HANDLE    NO-UNDO.
    
    DEFINE BUFFER bttQueryTable  FOR ttQueryTable.
    DEFINE BUFFER bttQueryColumn FOR ttQueryColumn.
    
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    
    iphQuery:QUERY-OPEN.
    iphQuery:GET-FIRST().
    IF NOT iphQuery:QUERY-OFF-END THEN DO:
        RUN pEmptyttColumn.
        FOR EACH bttQueryColumn
            WHERE bttQueryColumn.queryID EQ ttQueryName.queryID
            :
            ASSIGN
                hQueryBuf    = iphQuery:GET-BUFFER-HANDLE(bttQueryColumn.tableName)
                cFieldName   = ENTRY(2,bttQueryColumn.fieldName,".")
                .
            RUN pCreatettColumn (
                cFieldName,
                bttQueryColumn.sortOrder,
                YES,
                bttQueryColumn.fieldLabel,
                bttQueryColumn.dataType,
                bttQueryColumn.fieldFormat,
                hQueryBuf:BUFFER-FIELD(cFieldName):WIDTH,
                MAX(hQueryBuf:BUFFER-FIELD(cFieldName):WIDTH,LENGTH(hQueryBuf:BUFFER-FIELD(cFieldName):LABEL))
                ).
        END. /* do iColumn */
        OS-CREATE-DIR "users".
        OS-CREATE-DIR VALUE("users\" + USERID("ASI")).
        OS-CREATE-DIR VALUE("users\" + USERID("ASI") + "\Jasper").
        cJasperFile = "users\" + USERID("ASI") + "\"
                    + REPLACE(ttQueryName.queryName," ","")
                    + ".json"
                    .
        OUTPUT TO VALUE(cJasperFile).
        PUT UNFORMATTED
            "~{" SKIP
            FILL(" ",2)
            "~"" REPLACE(ttQueryName.queryName," ","_") "~": ~{" SKIP
            FILL(" ",4)
            "~"tt" REPLACE(ttQueryName.queryName," ","") "~": [" SKIP
            .
        REPEAT:
            PUT UNFORMATTED
                FILL(" ",6) "~{" SKIP
                .
            FOR EACH bttQueryColumn
                WHERE bttQueryColumn.queryID EQ ttQueryName.queryID
                   BY bttQueryColumn.sortOrder
                :
                ASSIGN
                    hQueryBuf    = iphQuery:GET-BUFFER-HANDLE(bttQueryColumn.tableName)
                    cFieldName   = ENTRY(2,bttQueryColumn.fieldName,".")
                    cBufferValue = fFormatValue(hQueryBuf, hQueryBuf:BUFFER-FIELD(cFieldName):NAME)
                    /* remove special characters with escape values */
                    cBufferValue = REPLACE(cBufferValue,"~&","~&amp;")
                    cBufferValue = REPLACE(cBufferValue,"~'","~&apos;")
                    cBufferValue = REPLACE(cBufferValue,"~"","~&quot;")
                    cBufferValue = REPLACE(cBufferValue,"<","~&lt;")
                    cBufferValue = REPLACE(cBufferValue,">","~&gt;")
                    cBufferValue = REPLACE(cBufferValue,"~\","~\~\")
                    .
                IF bttQuerycolumn.sortOrder GT 1 THEN
                PUT UNFORMATTED "," SKIP.
                PUT UNFORMATTED
                    FILL(" ",8)
                    "~"" cFieldName "~": ~""
                    IF cBufferValue NE "" THEN cBufferValue ELSE " "
                    "~""
                    .
            END. /* do iColumn */
            PUT UNFORMATTED SKIP FILL(" ",6) "}".
            iphQuery:GET-NEXT().
            IF iphQuery:QUERY-OFF-END THEN LEAVE.
            PUT UNFORMATTED "," SKIP.
        END. /* repeat */
        iphQuery:QUERY-CLOSE().
        PUT UNFORMATTED
            SKIP
            FILL(" ",4) "]" SKIP
            FILL(" ",2) "}" SKIP
            "}" SKIP
            .
        OUTPUT CLOSE.
        RUN pJasperCopy (cJasperFile).    
        RUN spJasperQuery (
            ipcType,
            ttQueryName.queryName,
            USERID("ASI"),
            OUTPUT cJasperFile
            ).
    END. /* if get-first is valid */
    iphQuery:QUERY-CLOSE().
    
    RUN LockWindowUpdate (0,OUTPUT i).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunQuery C-Win 
PROCEDURE pRunQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplRun  AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cDate   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cError  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParam  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtDate  AS DATE      NO-UNDO.
    DEFINE VARIABLE hBuffer AS HANDLE    NO-UNDO EXTENT 1000.
    DEFINE VARIABLE hQuery  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lOK     AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER ttQueryTable     FOR ttQueryTable.
    DEFINE BUFFER ttQueryParameter FOR ttQueryParameter.
    
    IF INDEX(queryStr,"[[") NE 0 THEN
    FOR EACH ttQueryParameter
        WHERE ttQueryParameter.queryID EQ ttQueryName.queryID
        :
        cParam = "[[" + ttQueryParameter.paramName + "]]".
        IF INDEX(queryStr,cParam) NE 0 THEN
        CASE ttQueryParameter.dataType:
            WHEN "Character" THEN
            queryStr = REPLACE(queryStr,cParam,"~"" + ttQueryParameter.initValue + "~"").
            WHEN "Date" THEN DO:
                dtDate = DATE(ttQueryParameter.initValue) NO-ERROR.
                cDate = IF dtDate EQ ? THEN "~?" ELSE STRING(dtDate,ttQueryParameter.paramFormat).
                queryStr = REPLACE(queryStr,cParam,cDate).
            END. /* date */
            WHEN "DateTime" THEN DO:
                dtDate = DATE(ttQueryParameter.initValue) NO-ERROR.
                cDate = IF dtDate EQ ? THEN "~?" ELSE STRING(dtDate,ttQueryParameter.paramFormat).
                queryStr = REPLACE(queryStr,cParam,cDate).
                queryStr = REPLACE(queryStr,cParam,ttQueryParameter.initValue).
            END. /* date */
            WHEN "Decimal" OR WHEN "Integer" OR WHEN "Logical" THEN
            queryStr = REPLACE(queryStr,cParam,ttQueryParameter.initValue).
        END CASE.
    END. /* for each */

    CREATE QUERY hQuery.
    FOR EACH ttQueryTable
        WHERE ttQueryTable.queryID EQ ttQueryName.queryID
        :
        idx = idx + 1.
        CREATE BUFFER hBuffer[idx] FOR TABLE ttQueryTable.tableName.
        hQuery:ADD-BUFFER(hBuffer[idx]).
    END. /* each ttquerytable */
    lOK = hQuery:QUERY-PREPARE(queryStr) NO-ERROR.
    IF lOK THEN DO:
        IF iplRun THEN DO:
            IF ipcType EQ "Results" THEN
            RUN pResultsBrowser (hQuery).
            ELSE
            RUN pResultsJasper (hQuery, ipcType).
        END. /* if run */
        ELSE
        MESSAGE
            "Query Syntax is Correct."        
        VIEW-AS ALERT-BOX TITLE "Query Syntax Check".
    END. /* if ok */
    ELSE DO:
        DO idx = 1 TO ERROR-STATUS:NUM-MESSAGES:
            cError = cError + ERROR-STATUS:GET-MESSAGE(idx) + CHR(10).
        END. /* do idx */
        MESSAGE cError VIEW-AS ALERT-BOX ERROR TITLE "Query Syntax Check".
    END. /* else */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveQuery C-Win 
PROCEDURE pSaveQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bttQueryName      FOR ttQueryName.
    DEFINE BUFFER bttQueryTable     FOR ttQueryTable.
    DEFINE BUFFER bttQueryWhere     FOR ttQueryWhere.
    DEFINE BUFFER bttQuerySort      FOR ttQuerySort.
    DEFINE BUFFER bttQueryColumn    FOR ttQueryColumn.
    DEFINE BUFFER bttQueryParameter FOR ttQueryParameter.
    
    OS-CREATE-DIR VALUE("users/" + USERID("ASI")).
    OS-CREATE-DIR VALUE("users/" + USERID("ASI") + "/Query").
    OUTPUT TO VALUE("users/" + USERID("ASI") + "/Query/ttQueryName.d").
    FOR EACH bttQueryName:
        EXPORT bttQueryName.
    END.
    OUTPUT CLOSE.
    OUTPUT TO VALUE("users/" + USERID("ASI") + "/Query/ttQueryTable.d").
    FOR EACH bttQueryTable:
        EXPORT bttQueryTable.
    END.
    OUTPUT CLOSE.
    OUTPUT TO VALUE("users/" + USERID("ASI") + "/Query/ttQueryWhere.d").
    FOR EACH bttQueryWhere:
        EXPORT bttQueryWhere.
    END.
    OUTPUT CLOSE.
    OUTPUT TO VALUE("users/" + USERID("ASI") + "/Query/ttQuerySort.d").
    FOR EACH bttQuerySort:
        EXPORT bttQuerySort.
    END.
    OUTPUT CLOSE.
    OUTPUT TO VALUE("users/" + USERID("ASI") + "/Query/ttQueryColumn.d").
    FOR EACH bttQueryColumn:
        EXPORT bttQueryColumn.
    END.
    OUTPUT CLOSE.
    OUTPUT TO VALUE("users/" + USERID("ASI") + "/Query/ttQueryParameter.d").
    FOR EACH bttQueryParameter:
        EXPORT bttQueryParameter.
    END.
    OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveResults C-Win 
PROCEDURE pSaveResults :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDBName    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTableName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dSize      AS DECIMAL   NO-UNDO.
    
    DO idx = 1 TO hQueryBrowse:NUM-COLUMNS:
        ASSIGN
            cDBName    = hQueryBrowse:GET-BROWSE-COLUMN(idx):DBNAME
            cTableName = hQueryBrowse:GET-BROWSE-COLUMN(idx):TABLE
            cFieldName = cTableName + "."
                       + hQueryBrowse:GET-BROWSE-COLUMN(idx):NAME
            dSize      = hQueryBrowse:GET-BROWSE-COLUMN(idx):WIDTH-CHARS
            .
        IF hQueryBrowse:GET-BROWSE-COLUMN(idx):INDEX NE 0 THEN
        cFieldName = cFieldName
                   + "["
                   + STRING(hQueryBrowse:GET-BROWSE-COLUMN(idx):INDEX)
                   + "]"
                   .
        FIND FIRST ttQueryColumn
             WHERE ttQueryColumn.queryID   EQ ttQueryName.queryID
               AND ttQueryColumn.tableDB   EQ cDBName
               AND ttQueryColumn.tableName EQ cTableName
               AND ttQueryColumn.fieldName EQ cFieldName
             NO-ERROR.
        IF NOT AVAILABLE ttQueryColumn THEN NEXT.
        IF ttQueryColumn.sortOrder  NE idx  OR
          (ttQueryColumn.columnSize NE 0    AND
           ttQueryColumn.columnSize NE dSize) THEN DO:
            fSetSaveButton (YES).
            ASSIGN
                ttQueryColumn.sortOrder  = idx
                ttQueryColumn.columnSize = dSize
                .
        END. /* if changes */
    END. /* do idx */
    IF lSave THEN
    {&OPEN-QUERY-queryColumnBrowse}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveSettings C-Win 
PROCEDURE pSaveSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    FIND FIRST user-print EXCLUSIVE-LOCK
         WHERE user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ USERID("ASI")
         NO-ERROR.
    IF NOT AVAILABLE user-print THEN DO:
        CREATE user-print.
        ASSIGN
            user-print.program-id = "{&program-id}"
            user-print.user-id    = USERID("ASI")
            .
    END. /* not avail */
    ASSIGN
        user-print.field-name  = ""
        user-print.field-value = ""
        user-print.field-label = ""
        .
    ASSIGN
        idx = idx + 1
        user-print.field-name[idx]  = "WindowColumn"
        user-print.field-label[idx] = "WindowColumn"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:COLUMN)
        idx = idx + 1
        user-print.field-name[idx]  = "WindowRow"
        user-print.field-label[idx] = "WindowRow"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:ROW)
        idx = idx + 1
        user-print.field-name[idx]  = "WindowWidth"
        user-print.field-label[idx] = "WindowWidth"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:WIDTH)
        idx = idx + 1
        user-print.field-name[idx]  = "WindowHeight"
        user-print.field-label[idx] = "WindowHeight"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:HEIGHT)
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetObjects C-Win 
PROCEDURE pSetObjects :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    &Scoped-define ROW 3.62

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN            
            /* field browse */
            RECT-FIELD:COL   = 40
            RECT-FIELD:ROW   = {&ROW}
            fieldSearch:COL  = RECT-FIELD:COL + 1
            fieldSearch:ROW  = RECT-FIELD:ROW + .24
            fieldMatches:COL = RECT-FIELD:COL + 24
            fieldMatches:ROW = RECT-FIELD:ROW + .24
            fieldBrowse:COL  = RECT-FIELD:COL
            fieldBrowse:ROW  = RECT-FIELD:ROW + 1.67
            
            /* query table browse */
            RECT-QUERYTABLE:COL  = 82
            RECT-QUERYTABLE:ROW  = {&ROW}
            findType:COL         = RECT-QUERYTABLE:COL + 2
            findType:ROW         = RECT-QUERYTABLE:ROW + .24
            queryTableBrowse:COL = RECT-QUERYTABLE:COL
            queryTableBrowse:ROW = RECT-QUERYTABLE:ROW + 1.67
            
            /* query sort browse */
            RECT-QUERYSORT:COL  = 82
            RECT-QUERYSORT:ROW  = {&ROW}
            sortType:COL        = RECT-QUERYSORT:COL + 9
            sortType:ROW        = RECT-QUERYSORT:ROW + .24
            querySortBrowse:COL = RECT-QUERYSORT:COL
            querySortBrowse:ROW = RECT-QUERYSORT:ROW + 1.67            

            /* query column browse */
            RECT-COLUMN:COL        = 82
            RECT-COLUMN:ROW        = {&ROW}
            columnSearch:COL       = RECT-COLUMN:COL + 1
            columnSearch:ROW       = RECT-COLUMN:ROW + .24
            columnMatches:COL      = RECT-COLUMN:COL + 66
            columnMatches:ROW      = RECT-COLUMN:ROW + .24
            queryColumnBrowse:COL  = RECT-COLUMN:COL
            queryColumnBrowse:ROW  = RECT-COLUMN:ROW + 1.67
            
            /* query parameter browse */
            cParamNameLabel:ROW      = 1
            cParamNameLabel:COL      = 95
            cParamName:ROW           = cParamNameLabel:ROW
            cParamName:COL           = cParamNameLabel:COL + 6
            cParamDataTypeLabel:ROW  = cParamNameLabel:ROW + 1.05
            cParamDataTypeLabel:COL  = cParamNameLabel:COL
            cParamDataType:ROW       = cParamDataTypeLabel:ROW
            cParamDataType:COL       = cParamNameLabel:COL + 6
            cParamFormatLabel:ROW    = cParamDataTypeLabel:ROW + 1.05
            cParamFormatLabel:COL    = cParamNameLabel:COL - 2
            cParamFormat:ROW         = cParamDataType:ROW + 1.05
            cParamFormat:COL         = cParamDataType:COL
            cParamInitLabel:ROW      = cParamFormatLabel:ROW + 1.05
            cParamInitLabel:COL      = cParamNameLabel:COL
            cParamInit:ROW           = cParamInitLabel:ROW
            cParamInit:COL           = cParamName:COL
            btnAddParamName:ROW      = cParamFormat:ROW
            btnAddParamName:COL      = cParamFormat:COL + 32
            RECT-PARAMETER:ROW       = {&ROW}
            parameterSearch:ROW      = RECT-PARAMETER:ROW + .24
            parameterMatches:ROW     = RECT-PARAMETER:ROW + .24
            queryParameterBrowse:ROW = RECT-PARAMETER:ROW + 1.67
            
            /* save button */
            RECT-SAVE:COL = 82
            RECT-SAVE:ROW = 1
            btnSave:ROW   = RECT-SAVE:ROW + .24
            btnSave:COL   = RECT-SAVE:COL + 1
            
            /*
            /* jasper objects */
            RECT-JASPER:COL = 104
            btnCSV:COL      = RECT-JASPER:COL + 1
            btnPDF:COL      = RECT-JASPER:COL + 6
            btnXLS:COL      = RECT-JASPER:COL + 11
            btnHTML:COL     = RECT-JASPER:COL + 16
            btnDOCX:COL     = RECT-JASPER:COL + 21
            btnView:COL     = RECT-JASPER:COL + 26
            */
            btnCloseResults:ROW = 1
            btnSaveResults:ROW  = 1
            .
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetOrder C-Win 
PROCEDURE pSetOrder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bttQueryColumn FOR ttQueryColumn.
    DEFINE BUFFER bttQuerySort   FOR ttQuerySort.
    DEFINE BUFFER bttQueryTable  FOR ttQueryTable.
    DEFINE BUFFER bttQueryWhere  FOR ttQueryWhere.
    
    CASE querySection:
        WHEN "Columns" THEN DO:
            {AOA/includes/pSetOrder.i "bttQueryColumn" "queryColumnBrowse"}
        END. /* columns */
        WHEN "Sort" THEN DO:
            {AOA/includes/pSetOrder.i "bttQuerySort" "querySortBrowse"}
        END. /* sort */
        WHEN "Table" THEN DO:
            {AOA/includes/pSetOrder.i "bttQueryTable" "queryTableBrowse"}
        END. /* table */
        WHEN "Where" OR WHEN "Parameters" THEN DO:
            {AOA/includes/pSetOrder.i "bttQueryWhere" "queryWhereBrowse"}
        END. /* where */
    END CASE.
    fSetSaveButton (YES).
    fShowQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinReSize C-Win 
PROCEDURE pWinReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    SESSION:SET-WAIT-STATE("General").
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            RECT-QUERYSTR:HIDDEN = YES
            btnSyntax:HIDDEN     = YES
            queryStr:HIDDEN      = YES
            .
        HIDE FRAME {&FRAME-NAME}.
        IF {&WINDOW-NAME}:HEIGHT LT 28.57 THEN
        {&WINDOW-NAME}:HEIGHT = 28.57.
        IF {&WINDOW-NAME}:WIDTH  LT 160   THEN
        {&WINDOW-NAME}:WIDTH  = 160.
        ASSIGN
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH
            FRAME {&FRAME-NAME}:HEIGHT         = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:WIDTH          = {&WINDOW-NAME}:WIDTH
            RECT-QUERYSTR:WIDTH                = FRAME {&FRAME-NAME}:WIDTH
                                               - RECT-QUERYSTR:COL
            RECT-QUERYSTR:ROW                  = FRAME {&FRAME-NAME}:HEIGHT
                                               - RECT-QUERYSTR:HEIGHT + 1
            btnSyntax:ROW                      = RECT-QUERYSTR:ROW + .72
            queryText:ROW                      = RECT-QUERYSTR:ROW - .23
            queryStr:ROW                       = RECT-QUERYSTR:ROW + .72
            queryStr:WIDTH                     = RECT-QUERYSTR:WIDTH - 7
            cParameter:ROW                     = RECT-QUERYSTR:ROW - 2.38
            cParameterLabel:ROW                = cParameter:ROW
            btnAddParameter:ROW                = cParameter:ROW            
            cConstant:ROW                      = RECT-QUERYSTR:ROW - 1.19
            cConstantLabel:ROW                 = cConstant:ROW
            btnAddConstant:ROW                 = cConstant:ROW
            /*
            btnNow:ROW                         = cConstant:ROW
            btntime:ROW                        = cConstant:ROW
            btnDateTime:ROW                    = cConstant:ROW
            btnToday:ROW                       = cConstant:ROW
            */
            BROWSE queryNameBrowse:HEIGHT      = FRAME {&FRAME-NAME}:HEIGHT
                                               - BROWSE queryNameBrowse:ROW + 1
            BROWSE queryNameBrowse:WIDTH       = IF querySection NE "Query" THEN 37
                                                 ELSE FRAME {&FRAME-NAME}:WIDTH
                                               - BROWSE queryNameBrowse:COL
            BROWSE tableBrowse:HEIGHT          = BROWSE queryNameBrowse:HEIGHT
            BROWSE fieldBrowse:HEIGHT          = BROWSE queryNameBrowse:HEIGHT
            BROWSE queryTableBrowse:HEIGHT     = RECT-QUERYSTR:ROW
                                               - BROWSE tableBrowse:ROW - .24
            BROWSE queryWhereBrowse:HEIGHT     = cParameter:ROW
                                               - BROWSE queryWhereBrowse:ROW - .20
            BROWSE querySortBrowse:HEIGHT      = BROWSE queryTableBrowse:HEIGHT
            BROWSE queryColumnBrowse:HEIGHT    = BROWSE queryNameBrowse:HEIGHT
            BROWSE queryParameterBrowse:HEIGHT = BROWSE queryNameBrowse:HEIGHT
            btnCloseResults:COL                = FRAME {&FRAME-NAME}:WIDTH
                                               - btnCloseResults:WIDTH + 1
            btnSaveResults:COL                 = btnCloseResults:COL
                                               - btnSaveResults:WIDTH
            .
        IF VALID-HANDLE(hQueryBrowse) THEN DO:
            ASSIGN
                hQueryBrowse:HEIGHT = FRAME {&FRAME-NAME}:HEIGHT
                hQueryBrowse:WIDTH  = FRAME {&FRAME-NAME}:WIDTH
                .
        END. /* if valid-handle */
        VIEW FRAME {&FRAME-NAME}.
        IF NOT CAN-DO("Query,Columns",querySection) THEN
        ASSIGN
            RECT-QUERYSTR:HIDDEN = NO
            btnSyntax:HIDDEN     = NO
            queryStr:HIDDEN      = NO
            .
        btnSyntax:MOVE-TO-TOP().
    END. /* do with */
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fFormatValue C-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fQueryStr C-Win 
FUNCTION fQueryStr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cQueryStr AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER ttQueryTable FOR ttQueryTable.
    DEFINE BUFFER ttQueryWhere FOR ttQueryWhere.
    DEFINE BUFFER ttQuerySort  FOR ttQuerySort.
    
    cQueryStr = "FOR".
    FOR EACH ttQueryTable
        WHERE ttQueryTable.queryID EQ ttQueryName.queryID
           BY ttQueryTable.sortOrder
        :
        cQueryStr = cQueryStr + " "
                  + ttQueryTable.tableFind + " "
                  + ttQueryTable.tableName + " "
                  .
        FOR EACH ttQueryWhere
            WHERE ttQueryWhere.queryID    EQ ttQueryTable.queryID
              AND ttQueryWhere.whereTable EQ ttQueryTable.tableName
               BY ttQueryWhere.sortOrder
            :
            cQueryStr = cQueryStr + ttQueryWhere.whereElement + " ".
        END. /* each ttquerywhere */
        cQueryStr = TRIM(cQueryStr) + ", ".
    END. /* each ttquerytable */
    cQueryStr = TRIM(cQueryStr,", ").
    FOR EACH ttQuerySort
        WHERE ttQuerySort.queryID EQ ttQueryName.queryID
           BY ttQuerySort.sortOrder
        :
        cQueryStr = cQueryStr + " BY " + ttQuerySort.sortBy.
    END. /* each ttquerysort */
    
    RETURN cQueryStr.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fSetSaveButton C-Win 
FUNCTION fSetSaveButton RETURNS LOGICAL
  (iplSave AS LOGICAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            lSave  = iplSave
            RECT-SAVE:HIDDEN = NOT lSave
            btnSave:HIDDEN   = NOT lSave
            .
    END. /* with frame */
    RETURN lSave.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fShowQuery C-Win 
FUNCTION fShowQuery RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    ASSIGN
        queryStr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fQueryStr()
        queryStr
        ttQueryName.queryStr = queryStr
        .
    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fSortBy C-Win 
FUNCTION fSortBy RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    RETURN TRIM(ttQuerySort.tableName + "."
              + TRIM(ttQuerySort.fieldName + " "
              + STRING(ttQuerySort.sortAsc,"/DESCENDING")))
              .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

