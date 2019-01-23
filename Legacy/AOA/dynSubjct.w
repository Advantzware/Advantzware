&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: AOA/dynSubjct.w

  Description: Dynamic Subject Query Builder

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

&Scoped-define program-id dynSubjct.

&Scoped-define transPanel btnUpdate btnAdd btnCopy btnDelete btnReset btnCancel
&Scoped-define transInit btnUpdate btnAdd btnCopy btnDelete
&Scoped-define transUpdate btnUpdate btnReset btnCancel
&Scoped-define parameterSection RECT-PARAM ~
paramSetSearch paramSetMatches ~
paramSetBrowse subjectParamSetBrowse

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cDataType          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFormat            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLabel             AS CHARACTER NO-UNDO.
DEFINE VARIABLE hAppSrvBin         AS HANDLE    NO-UNDO.
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
DEFINE TEMP-TABLE ttSubject         NO-UNDO LIKE dynSubject.
DEFINE TEMP-TABLE ttSubjectTable    NO-UNDO LIKE dynSubjectTable.
DEFINE TEMP-TABLE ttSubjectWhere    NO-UNDO LIKE dynSubjectWhere.
DEFINE TEMP-TABLE ttSubjectSort     NO-UNDO LIKE dynSubjectSort.
DEFINE TEMP-TABLE ttSubjectColumn   NO-UNDO LIKE dynSubjectColumn.
DEFINE TEMP-TABLE ttSubjectParamSet NO-UNDO LIKE dynSubjectParamSet.
DEFINE TEMP-TABLE ttAction NO-UNDO
    FIELD paramWidget    AS HANDLE
    FIELD paramID        AS INTEGER
    FIELD actionParamID  AS INTEGER
    FIELD action         AS CHARACTER
    FIELD initializeProc AS CHARACTER
    FIELD validateProc   AS CHARACTER
        INDEX paramWidget IS PRIMARY paramWidget
        INDEX paramID paramID
        INDEX actionParamID actionParamID action
        .
DEFINE TEMP-TABLE ttGroupCalc NO-UNDO 
    FIELD subjectID AS INTEGER
    FIELD fieldName AS CHARACTER
    FIELD groupName AS CHARACTER 
    FIELD calcType  AS CHARACTER
        INDEX ttCalcGroup IS PRIMARY
            subjectID
            fieldName
            groupName
            calcType
            . 
RUN AOA\appServer\aoaBin.p PERSISTENT SET hAppSrvBin
SESSION:ADD-SUPER-PROCEDURE (hAppSrvBin).
RUN AOA\aoaJasper.p PERSISTENT SET hJasper
SESSION:ADD-SUPER-PROCEDURE (hJasper).

{methods/lockWindowUpdate.i}

iUserSecurityLevel = DYNAMIC-FUNCTION("sfUserSecurityLevel").

/* function fDateOptions */
{AOA/includes/fDateOptions.i}
/* function fDateOptionValue */
{AOA/includes/fDateOptionValue.i}

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
&Scoped-define INTERNAL-TABLES ttField dynParamSet ttSubject ~
ttSubjectColumn ttSubjectParamSet ttSubjectSort ttSubjectTable ~
ttSubjectWhere ttTable

/* Definitions for BROWSE fieldBrowse                                   */
&Scoped-define FIELDS-IN-QUERY-fieldBrowse ttField.fieldName ttField.fieldLabel   
&Scoped-define ENABLED-FIELDS-IN-QUERY-fieldBrowse   
&Scoped-define SELF-NAME fieldBrowse
&Scoped-define QUERY-STRING-fieldBrowse FOR EACH ttField WHERE (fieldMatches EQ NO AND (ttField.fieldName BEGINS fieldSearch OR ttField.fieldLabel BEGINS fieldSearch)) OR (fieldMatches EQ YES AND (ttField.fieldName MATCHES "*" + fieldSearch + "*" OR ttField.fieldLabel MATCHES "*" + fieldSearch + "*"))  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-fieldBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttField WHERE (fieldMatches EQ NO AND (ttField.fieldName BEGINS fieldSearch OR ttField.fieldLabel BEGINS fieldSearch)) OR (fieldMatches EQ YES AND (ttField.fieldName MATCHES "*" + fieldSearch + "*" OR ttField.fieldLabel MATCHES "*" + fieldSearch + "*"))  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-fieldBrowse ttField
&Scoped-define FIRST-TABLE-IN-QUERY-fieldBrowse ttField


/* Definitions for BROWSE paramSetBrowse                                */
&Scoped-define FIELDS-IN-QUERY-paramSetBrowse dynParamSet.setName dynParamSet.paramSetID   
&Scoped-define ENABLED-FIELDS-IN-QUERY-paramSetBrowse   
&Scoped-define SELF-NAME paramSetBrowse
&Scoped-define QUERY-STRING-paramSetBrowse FOR EACH dynParamSet WHERE (paramSetMatches EQ NO AND dynParamSet.setName BEGINS paramSetSearch) OR (paramSetMatches EQ YES AND dynParamSet.setName MATCHES "*" + paramSetSearch + "*")  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-paramSetBrowse OPEN QUERY {&SELF-NAME} FOR EACH dynParamSet WHERE (paramSetMatches EQ NO AND dynParamSet.setName BEGINS paramSetSearch) OR (paramSetMatches EQ YES AND dynParamSet.setName MATCHES "*" + paramSetSearch + "*")  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-paramSetBrowse dynParamSet
&Scoped-define FIRST-TABLE-IN-QUERY-paramSetBrowse dynParamSet


/* Definitions for BROWSE subjectBrowse                                 */
&Scoped-define FIELDS-IN-QUERY-subjectBrowse ttSubject.subjectName ttSubject.isActive ttSubject.subjectID ttSubject.subjectType ttSubject.module ttSubject.user-id ttSubject.securityLevel ttSubject.subjectHeight ttSubject.subjectWidth   
&Scoped-define ENABLED-FIELDS-IN-QUERY-subjectBrowse   
&Scoped-define SELF-NAME subjectBrowse
&Scoped-define QUERY-STRING-subjectBrowse FOR EACH ttSubject WHERE (subjectMatches EQ NO  AND ttSubject.subjectName BEGINS subjectSearch)    OR (subjectMatches EQ YES AND ttSubject.subjectName MATCHES "*" + subjectSearch + "*")
&Scoped-define OPEN-QUERY-subjectBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttSubject WHERE (subjectMatches EQ NO  AND ttSubject.subjectName BEGINS subjectSearch)    OR (subjectMatches EQ YES AND ttSubject.subjectName MATCHES "*" + subjectSearch + "*").
&Scoped-define TABLES-IN-QUERY-subjectBrowse ttSubject
&Scoped-define FIRST-TABLE-IN-QUERY-subjectBrowse ttSubject


/* Definitions for BROWSE subjectColumnBrowse                           */
&Scoped-define FIELDS-IN-QUERY-subjectColumnBrowse ttSubjectColumn.fieldName ttSubjectColumn.fieldLabel ttSubjectColumn.isGroup ttSubjectColumn.groupLabel ttSubjectColumn.fieldFormat ttSubjectColumn.groupCalc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-subjectColumnBrowse ttSubjectColumn.fieldLabel ttSubjectColumn.isGroup ttSubjectColumn.groupLabel ttSubjectColumn.fieldFormat   
&Scoped-define ENABLED-TABLES-IN-QUERY-subjectColumnBrowse ttSubjectColumn
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-subjectColumnBrowse ttSubjectColumn
&Scoped-define SELF-NAME subjectColumnBrowse
&Scoped-define QUERY-STRING-subjectColumnBrowse FOR EACH ttSubjectColumn WHERE ttSubjectColumn.subjectID EQ ttSubject.subjectID   AND ((columnMatches EQ NO  AND ttSubjectColumn.fieldName BEGINS columnSearch)    OR (columnMatches EQ YES AND ttSubjectColumn.fieldName MATCHES "*" + columnSearch + "*")) BY ttSubjectColumn.sortOrder
&Scoped-define OPEN-QUERY-subjectColumnBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttSubjectColumn WHERE ttSubjectColumn.subjectID EQ ttSubject.subjectID   AND ((columnMatches EQ NO  AND ttSubjectColumn.fieldName BEGINS columnSearch)    OR (columnMatches EQ YES AND ttSubjectColumn.fieldName MATCHES "*" + columnSearch + "*")) BY ttSubjectColumn.sortOrder.
&Scoped-define TABLES-IN-QUERY-subjectColumnBrowse ttSubjectColumn
&Scoped-define FIRST-TABLE-IN-QUERY-subjectColumnBrowse ttSubjectColumn


/* Definitions for BROWSE subjectParamSetBrowse                         */
&Scoped-define FIELDS-IN-QUERY-subjectParamSetBrowse dynParamSet.setName dynParamSet.setTitle ttSubjectParamSet.paramSetID ttSubjectParamSet.setRow ttSubjectParamSet.setCol   
&Scoped-define ENABLED-FIELDS-IN-QUERY-subjectParamSetBrowse   
&Scoped-define SELF-NAME subjectParamSetBrowse
&Scoped-define QUERY-STRING-subjectParamSetBrowse FOR EACH ttSubjectParamSet WHERE ttSubjectParamSet.subjectID EQ ttSubject.subjectID, ~
       FIRST dynParamSet NO-LOCK WHERE dynParamSet.paramSetID EQ ttSubjectParamSet.paramSetID
&Scoped-define OPEN-QUERY-subjectParamSetBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttSubjectParamSet WHERE ttSubjectParamSet.subjectID EQ ttSubject.subjectID, ~
       FIRST dynParamSet NO-LOCK WHERE dynParamSet.paramSetID EQ ttSubjectParamSet.paramSetID.
&Scoped-define TABLES-IN-QUERY-subjectParamSetBrowse ttSubjectParamSet ~
dynParamSet
&Scoped-define FIRST-TABLE-IN-QUERY-subjectParamSetBrowse ttSubjectParamSet
&Scoped-define SECOND-TABLE-IN-QUERY-subjectParamSetBrowse dynParamSet


/* Definitions for BROWSE subjectSortBrowse                             */
&Scoped-define FIELDS-IN-QUERY-subjectSortBrowse ttSubjectSort.fieldLabel ttSubjectSort.sortBy   
&Scoped-define ENABLED-FIELDS-IN-QUERY-subjectSortBrowse   
&Scoped-define SELF-NAME subjectSortBrowse
&Scoped-define QUERY-STRING-subjectSortBrowse FOR EACH ttSubjectSort WHERE ttSubjectSort.subjectID EQ ttSubject.subjectID BY ttSubjectSort.sortOrder
&Scoped-define OPEN-QUERY-subjectSortBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttSubjectSort WHERE ttSubjectSort.subjectID EQ ttSubject.subjectID BY ttSubjectSort.sortOrder.
&Scoped-define TABLES-IN-QUERY-subjectSortBrowse ttSubjectSort
&Scoped-define FIRST-TABLE-IN-QUERY-subjectSortBrowse ttSubjectSort


/* Definitions for BROWSE subjectTableBrowse                            */
&Scoped-define FIELDS-IN-QUERY-subjectTableBrowse ttSubjectTable.tableFind ttSubjectTable.tableName ttSubjectTable.useIndex ttSubjectTable.tableDscr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-subjectTableBrowse   
&Scoped-define SELF-NAME subjectTableBrowse
&Scoped-define QUERY-STRING-subjectTableBrowse FOR EACH ttSubjectTable WHERE ttSubjectTable.subjectID EQ ttSubject.subjectID BY ttSubjectTable.sortOrder
&Scoped-define OPEN-QUERY-subjectTableBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttSubjectTable WHERE ttSubjectTable.subjectID EQ ttSubject.subjectID BY ttSubjectTable.sortOrder.
&Scoped-define TABLES-IN-QUERY-subjectTableBrowse ttSubjectTable
&Scoped-define FIRST-TABLE-IN-QUERY-subjectTableBrowse ttSubjectTable


/* Definitions for BROWSE subjectWhereBrowse                            */
&Scoped-define FIELDS-IN-QUERY-subjectWhereBrowse ttSubjectWhere.whereElement ttSubjectWhere.fieldLabel ttSubjectWhere.dataType   
&Scoped-define ENABLED-FIELDS-IN-QUERY-subjectWhereBrowse   
&Scoped-define SELF-NAME subjectWhereBrowse
&Scoped-define QUERY-STRING-subjectWhereBrowse FOR EACH ttSubjectWhere WHERE ttSubjectWhere.subjectID EQ ttSubject.subjectID AND ttSubjectWhere.whereTable EQ tableList BY ttSubjectWhere.sortOrder
&Scoped-define OPEN-QUERY-subjectWhereBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttSubjectWhere WHERE ttSubjectWhere.subjectID EQ ttSubject.subjectID AND ttSubjectWhere.whereTable EQ tableList BY ttSubjectWhere.sortOrder.
&Scoped-define TABLES-IN-QUERY-subjectWhereBrowse ttSubjectWhere
&Scoped-define FIRST-TABLE-IN-QUERY-subjectWhereBrowse ttSubjectWhere


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
    ~{&OPEN-QUERY-paramSetBrowse}~
    ~{&OPEN-QUERY-subjectBrowse}~
    ~{&OPEN-QUERY-subjectColumnBrowse}~
    ~{&OPEN-QUERY-subjectParamSetBrowse}~
    ~{&OPEN-QUERY-subjectSortBrowse}~
    ~{&OPEN-QUERY-subjectTableBrowse}~
    ~{&OPEN-QUERY-subjectWhereBrowse}~
    ~{&OPEN-QUERY-tableBrowse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnGroupCalc subjectSection btnNow btnToday ~
btnTime btnDateTime subjectSearch subjectMatches tableSearch tableMatches ~
tableList btnOF tableListOf btnWhere btnMatches subjectBrowse tableBrowse ~
subjectWhereBrowse btnBegins btnAND btnOR btnYes btnNo btnEQ btnNE ~
paramSetSearch paramSetMatches fieldSearch fieldMatches sortType btnLT ~
btnGT paramSetBrowse fieldBrowse subjectSortBrowse btnLE btnGE btnPlus ~
btnMinus btnMultiply btnDivide btnDate cUseIndex columnSearch columnMatches ~
btnDec subjectTableBrowse subjectColumnBrowse btnInt btnStr btnSubstr ~
cParameter btnOpen btnClose findType cConstant btnPeriod btnDouble btnComma ~
btnSingle queryStr subjectParamSetBrowse btnResults btnAddUseIndex ~
btnRemoveUseIndex btnAddParameter btnSave btnRemoveSelection btnSyntax ~
btnAddSelections btnMoveDown btnAddConstant btnMoveUp btnRemove btnHTML ~
btnView btnPDF btnDOCX btnCSV btnXLS btnAdd btnCopy btnDelete btnUpdate ~
cParameterLabel cConstantLabel 
&Scoped-Define DISPLAYED-OBJECTS subjectSection subjectSearch ~
subjectMatches tableSearch tableMatches tableList tableListOf ~
paramSetSearch paramSetMatches fieldSearch fieldMatches sortType cUseIndex ~
columnSearch columnMatches cParameter findType cConstant queryStr ~
cUseIndexLabel cParameterLabel cConstantLabel queryText 

/* Custom List Definitions                                              */
/* allSection,tableSection,whereSection,sortSection,columnsSection,subjectSection */
&Scoped-define allSection btnGroupCalc RECT-TABLE RECT-FIELD ~
RECT-QUERYTABLE RECT-QUERYSORT RECT-QUERYSTR RECT-COLUMN RECT-PANEL ~
RECT-SAVE RECT-PARAM btnNow btnToday btnTime btnDateTime tableSearch ~
tableMatches tableList btnOF tableListOf btnWhere btnMatches tableBrowse ~
subjectWhereBrowse btnBegins btnAND btnOR btnYes btnNo btnEQ btnNE ~
paramSetSearch paramSetMatches fieldSearch fieldMatches sortType btnLT ~
btnGT paramSetBrowse fieldBrowse subjectSortBrowse btnLE btnGE btnPlus ~
btnMinus btnMultiply btnDivide btnDate cUseIndex columnSearch columnMatches ~
btnDec subjectTableBrowse subjectColumnBrowse btnInt btnStr btnSubstr ~
cParameter btnOpen btnClose findType cConstant btnPeriod btnDouble btnComma ~
btnSingle queryStr subjectParamSetBrowse btnAddUseIndex btnRemoveUseIndex ~
btnAddParameter btnSave btnRemoveSelection btnSyntax btnAddSelections ~
btnMoveDown btnAddConstant btnCancel btnMoveUp btnRemove btnAdd btnCopy ~
btnDelete btnReset btnUpdate cUseIndexLabel cParameterLabel cConstantLabel ~
queryText 
&Scoped-define tableSection btnGroupCalc RECT-TABLE RECT-QUERYTABLE ~
RECT-QUERYSTR tableSearch tableMatches tableBrowse cUseIndex ~
subjectTableBrowse findType queryStr btnAddUseIndex btnRemoveUseIndex ~
btnRemoveSelection btnSyntax btnAddSelections btnMoveDown btnMoveUp ~
btnRemove cUseIndexLabel queryText 
&Scoped-define whereSection btnGroupCalc RECT-FIELD RECT-QUERYSTR ~
RECT-PARAM btnNow btnToday btnTime btnDateTime tableList btnOF tableListOf ~
btnWhere btnMatches subjectWhereBrowse btnBegins btnAND btnOR btnYes btnNo ~
btnEQ btnNE paramSetSearch paramSetMatches fieldSearch fieldMatches btnLT ~
btnGT fieldBrowse btnLE btnGE btnPlus btnMinus btnMultiply btnDivide ~
btnDate btnDec btnInt btnStr btnSubstr cParameter btnOpen btnClose ~
cConstant btnPeriod btnDouble btnComma btnSingle queryStr btnAddParameter ~
btnRemoveSelection btnSyntax btnAddSelections btnMoveDown btnAddConstant ~
btnMoveUp btnRemove cParameterLabel cConstantLabel queryText 
&Scoped-define sortSection btnGroupCalc RECT-FIELD RECT-QUERYSORT ~
RECT-QUERYSTR RECT-PARAM paramSetSearch paramSetMatches fieldSearch ~
fieldMatches sortType fieldBrowse subjectSortBrowse queryStr ~
btnRemoveSelection btnSyntax btnAddSelections btnMoveDown btnMoveUp ~
btnRemove queryText 
&Scoped-define columnsSection btnGroupCalc RECT-FIELD RECT-COLUMN ~
RECT-PARAM paramSetSearch paramSetMatches fieldSearch fieldMatches ~
fieldBrowse columnSearch columnMatches subjectColumnBrowse ~
btnRemoveSelection btnAddSelections btnMoveDown btnMoveUp btnRemove 
&Scoped-define subjectSection RECT-PANEL btnCancel btnAdd btnCopy btnDelete ~
btnReset btnUpdate 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCreateLabel C-Win 
FUNCTION fCreateLabel RETURNS HANDLE
  (ipcPool AS CHARACTER, iphFrame AS HANDLE, ipcLabel AS CHARACTER, ipdRow AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fFormatValue C-Win 
FUNCTION fFormatValue RETURNS CHARACTER
  (iphTable AS HANDLE, ipcField AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetUseIndex C-Win 
FUNCTION fGetUseIndex RETURNS CHARACTER
  (ipcTableDB AS CHARACTER, ipcTableName AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fJasperFields C-Win 
FUNCTION fJasperFields RETURNS CHARACTER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fJasperGroupCalc C-Win 
FUNCTION fJasperGroupCalc RETURNS CHARACTER
  (ipcField AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fJasperGroups C-Win 
FUNCTION fJasperGroups RETURNS CHARACTER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fJasperVariables C-Win 
FUNCTION fJasperVariables RETURNS CHARACTER
  (  ) FORWARD.

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

DEFINE BUTTON btnAddSelections 
     IMAGE-UP FILE "Graphics/16x16/next.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Add Selection" 
     SIZE 4.4 BY 1 TOOLTIP "Add Selections".

DEFINE BUTTON btnAddUseIndex 
     IMAGE-UP FILE "Graphics/16x16/navigate_plus.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Add Use Index" 
     SIZE 5 BY 1.05 TOOLTIP "Add Use Index".

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

DEFINE BUTTON btnGroupCalc 
     IMAGE-UP FILE "Graphics/16x16/spreadsheet_sum.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Group Calculations".

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

DEFINE BUTTON btnRemoveUseIndex 
     IMAGE-UP FILE "Graphics/16x16/navigate_cross.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Remove Use Index" 
     SIZE 5 BY 1.05 TOOLTIP "Remove Use Index".

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "Graphics/32x32/undo_32.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/undo_32_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91 TOOLTIP "Reset".

DEFINE BUTTON btnResults 
     IMAGE-UP FILE "AOA/images/media_play.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Results" 
     SIZE 4.4 BY 1 TOOLTIP "Subject Results".

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Save" 
     SIZE 8 BY 1.91 TOOLTIP "Save".

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

DEFINE VARIABLE cParameter AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX SORT INNER-LINES 6
     DROP-DOWN-LIST
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE cUseIndex AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

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

DEFINE VARIABLE cParameterLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Parameter:" 
      VIEW-AS TEXT 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE cUseIndexLabel AS CHARACTER FORMAT "X(256)":U INITIAL "USE-INDEX:" 
      VIEW-AS TEXT 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fieldSearch AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE paramSetSearch AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE queryText AS CHARACTER FORMAT "X(256)":U INITIAL " Query" 
      VIEW-AS TEXT 
     SIZE 8 BY .62
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE VARIABLE subjectSearch AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

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

DEFINE VARIABLE sortType AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ascending", yes,
"Descending", no
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE subjectSection AS CHARACTER INITIAL "Subject" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Sub&ject", "Subject",
"&Table", "Table",
"&Where", "Where",
"S&ort", "Sort",
"&Columns", "Columns",
"&Parameters", "Parameters"
     SIZE 72 BY 1
     FONT 6 NO-UNDO.

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

DEFINE RECTANGLE RECT-PARAM
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

DEFINE VARIABLE paramSetMatches AS LOGICAL INITIAL yes 
     LABEL "Matches" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE subjectMatches AS LOGICAL INITIAL yes 
     LABEL "Matches" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE tableMatches AS LOGICAL INITIAL no 
     LABEL "Matches" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY 1 NO-UNDO.

DEFINE BUTTON btnCloseParam 
     IMAGE-UP FILE "AOA/images/navigate_cross.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Close Results" 
     SIZE 4.4 BY .95 TOOLTIP "Close Results".

DEFINE BUTTON btnRunResults 
     IMAGE-UP FILE "AOA/images/navigate_check.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Save Results" 
     SIZE 4.4 BY .95 TOOLTIP "Save Results".

DEFINE BUTTON btnCloseResults 
     IMAGE-UP FILE "AOA/images/navigate_cross.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Close Results" 
     SIZE 4.4 BY 1 TOOLTIP "Close Results".

DEFINE BUTTON btnSaveResults 
     IMAGE-UP FILE "AOA/images/navigate_check.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Save Results" 
     SIZE 4.4 BY 1 TOOLTIP "Save Results".

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fieldBrowse FOR 
      ttField SCROLLING.

DEFINE QUERY paramSetBrowse FOR 
      dynParamSet SCROLLING.

DEFINE QUERY subjectBrowse FOR 
      ttSubject SCROLLING.

DEFINE QUERY subjectColumnBrowse FOR 
      ttSubjectColumn SCROLLING.

DEFINE QUERY subjectParamSetBrowse FOR 
      ttSubjectParamSet, 
      dynParamSet SCROLLING.

DEFINE QUERY subjectSortBrowse FOR 
      ttSubjectSort SCROLLING.

DEFINE QUERY subjectTableBrowse FOR 
      ttSubjectTable SCROLLING.

DEFINE QUERY subjectWhereBrowse FOR 
      ttSubjectWhere SCROLLING.

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

DEFINE BROWSE paramSetBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS paramSetBrowse C-Win _FREEFORM
  QUERY paramSetBrowse DISPLAY
      dynParamSet.setName
dynParamSet.paramSetID
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 36 BY 4.19
         TITLE "Available Parameter Sets".

DEFINE BROWSE subjectBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS subjectBrowse C-Win _FREEFORM
  QUERY subjectBrowse DISPLAY
      ttSubject.subjectName
ttSubject.isActive VIEW-AS TOGGLE-BOX
ttSubject.subjectID
ttSubject.subjectType
ttSubject.module
ttSubject.user-id
ttSubject.securityLevel
ttSubject.subjectHeight
ttSubject.subjectWidth
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 37 BY 4.19
         TITLE "Subject" ROW-HEIGHT-CHARS .62.

DEFINE BROWSE subjectColumnBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS subjectColumnBrowse C-Win _FREEFORM
  QUERY subjectColumnBrowse DISPLAY
      ttSubjectColumn.fieldName
ttSubjectColumn.fieldLabel
ttSubjectColumn.isGroup VIEW-AS TOGGLE-BOX
ttSubjectColumn.groupLabel
ttSubjectColumn.fieldFormat
ttSubjectColumn.groupCalc
ENABLE
ttSubjectColumn.fieldLabel
ttSubjectColumn.isGroup
ttSubjectColumn.groupLabel
ttSubjectColumn.fieldFormat
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 78 BY 4.19
         TITLE "Subject Columns" ROW-HEIGHT-CHARS .62.

DEFINE BROWSE subjectParamSetBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS subjectParamSetBrowse C-Win _FREEFORM
  QUERY subjectParamSetBrowse DISPLAY
      dynParamSet.setName
dynParamSet.setTitle
ttSubjectParamSet.paramSetID
ttSubjectParamSet.setRow
ttSubjectParamSet.setCol
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 83 BY 4.19
         TITLE "Subject Parameter Sets".

DEFINE BROWSE subjectSortBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS subjectSortBrowse C-Win _FREEFORM
  QUERY subjectSortBrowse DISPLAY
      ttSubjectSort.fieldLabel
ttSubjectSort.sortBy
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 78 BY 4.19
         TITLE "Subject Sort".

DEFINE BROWSE subjectTableBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS subjectTableBrowse C-Win _FREEFORM
  QUERY subjectTableBrowse DISPLAY
      ttSubjectTable.tableFind
ttSubjectTable.tableName
ttSubjectTable.useIndex
ttSubjectTable.tableDscr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 78 BY 4.19
         TITLE "Subject Tables" ROW-HEIGHT-CHARS .62.

DEFINE BROWSE subjectWhereBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS subjectWhereBrowse C-Win _FREEFORM
  QUERY subjectWhereBrowse DISPLAY
      ttSubjectWhere.whereElement
ttSubjectWhere.fieldLabel
ttSubjectWhere.dataType
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 67 BY 4.19
         TITLE "Subject Where".

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
     btnGroupCalc AT ROW 5.52 COL 77 HELP
          "Group Calculations" WIDGET-ID 272
     subjectSection AT ROW 2.19 COL 3 HELP
          "Select Section" NO-LABEL WIDGET-ID 30
     btnNow AT ROW 2.91 COL 117 WIDGET-ID 194
     btnToday AT ROW 2.91 COL 126 WIDGET-ID 190
     btnTime AT ROW 2.91 COL 136 WIDGET-ID 192
     btnDateTime AT ROW 2.91 COL 146 WIDGET-ID 196
     subjectSearch AT ROW 3.86 COL 3 NO-LABEL WIDGET-ID 16
     subjectMatches AT ROW 3.86 COL 26 HELP
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
     subjectBrowse AT ROW 5.29 COL 2 WIDGET-ID 600
     tableBrowse AT ROW 5.29 COL 40 WIDGET-ID 200
     subjectWhereBrowse AT ROW 5.29 COL 82 WIDGET-ID 800
     btnBegins AT ROW 5.29 COL 150 WIDGET-ID 84
     btnAND AT ROW 6.48 COL 150 WIDGET-ID 80
     btnOR AT ROW 6.48 COL 155 WIDGET-ID 82
     btnYes AT ROW 7.67 COL 150 WIDGET-ID 172
     btnNo AT ROW 7.67 COL 155 WIDGET-ID 174
     btnEQ AT ROW 8.86 COL 150 WIDGET-ID 68
     btnNE AT ROW 8.86 COL 155 WIDGET-ID 70
     paramSetSearch AT ROW 9.81 COL 3 HELP
          "Enter Field Search" NO-LABEL WIDGET-ID 258
     paramSetMatches AT ROW 9.81 COL 26 HELP
          "Select for Table Search Matches" WIDGET-ID 256
     fieldSearch AT ROW 9.81 COL 39 COLON-ALIGNED HELP
          "Enter Field Search" NO-LABEL WIDGET-ID 50
     fieldMatches AT ROW 9.81 COL 64 HELP
          "Select for Table Search Matches" WIDGET-ID 52
     sortType AT ROW 9.81 COL 90 NO-LABEL WIDGET-ID 22
     btnLT AT ROW 10.05 COL 150 WIDGET-ID 72
     btnGT AT ROW 10.05 COL 155 WIDGET-ID 74
     paramSetBrowse AT ROW 11 COL 2 WIDGET-ID 1000
     fieldBrowse AT ROW 11 COL 40 WIDGET-ID 700
     subjectSortBrowse AT ROW 11 COL 82 WIDGET-ID 500
     btnLE AT ROW 11.24 COL 150 WIDGET-ID 76
     btnGE AT ROW 11.24 COL 155 WIDGET-ID 78
     btnPlus AT ROW 12.43 COL 150 WIDGET-ID 158
     btnMinus AT ROW 12.43 COL 155 WIDGET-ID 156
     btnMultiply AT ROW 13.62 COL 150 WIDGET-ID 160
     btnDivide AT ROW 13.62 COL 155 WIDGET-ID 154
     btnDate AT ROW 14.81 COL 150 WIDGET-ID 166
     cUseIndex AT ROW 15.52 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 262
     columnSearch AT ROW 15.52 COL 70 COLON-ALIGNED HELP
          "Enter Column Search" NO-LABEL WIDGET-ID 112
     columnMatches AT ROW 15.52 COL 137 HELP
          "Select for Column Search Matches" WIDGET-ID 110
     btnDec AT ROW 16 COL 150 WIDGET-ID 164
     subjectTableBrowse AT ROW 16.71 COL 2 WIDGET-ID 400
     subjectColumnBrowse AT ROW 16.71 COL 71 WIDGET-ID 900
     btnInt AT ROW 17.19 COL 150 WIDGET-ID 162
     btnStr AT ROW 18.38 COL 150 WIDGET-ID 168
     btnSubstr AT ROW 19.57 COL 150 WIDGET-ID 170
     cParameter AT ROW 20.76 COL 91 COLON-ALIGNED HELP
          "Select Parameter Type" NO-LABEL WIDGET-ID 204
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     btnOpen AT ROW 20.76 COL 150 WIDGET-ID 94
     btnClose AT ROW 20.76 COL 155 WIDGET-ID 96
     findType AT ROW 21.24 COL 4 NO-LABEL WIDGET-ID 26
     cConstant AT ROW 21.95 COL 91 COLON-ALIGNED NO-LABEL WIDGET-ID 176
     btnPeriod AT ROW 21.95 COL 150 WIDGET-ID 236
     btnDouble AT ROW 21.95 COL 152.4 WIDGET-ID 240
     btnComma AT ROW 21.95 COL 155 WIDGET-ID 242
     btnSingle AT ROW 21.95 COL 157.4 WIDGET-ID 244
     queryStr AT ROW 23.86 COL 83 NO-LABEL WIDGET-ID 4
     subjectParamSetBrowse AT ROW 25.29 COL 2 WIDGET-ID 1100
     btnResults AT ROW 1.24 COL 131 HELP
          "Jasper Viewer" WIDGET-ID 250
     btnAddUseIndex AT ROW 15.52 COL 60 WIDGET-ID 268
     btnRemoveUseIndex AT ROW 15.52 COL 65 WIDGET-ID 270
     btnAddParameter AT ROW 20.76 COL 145 WIDGET-ID 208
     btnSave AT ROW 22.91 COL 3 HELP
          "Update/Save" WIDGET-ID 248
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
     cUseIndexLabel AT ROW 15.52 COL 22 COLON-ALIGNED NO-LABEL WIDGET-ID 266
     cParameterLabel AT ROW 20.76 COL 80 COLON-ALIGNED NO-LABEL WIDGET-ID 206
     cConstantLabel AT ROW 21.95 COL 81 COLON-ALIGNED NO-LABEL WIDGET-ID 178
     queryText AT ROW 22.91 COL 78 COLON-ALIGNED NO-LABEL WIDGET-ID 108
     "SECTIONS" VIEW-AS TEXT
          SIZE 11 BY .91 AT ROW 1.24 COL 33 WIDGET-ID 36
          FGCOLOR 9 FONT 6
     RECT-SECTION AT ROW 1 COL 2 WIDGET-ID 42
     RECT-QUERY AT ROW 3.62 COL 2 WIDGET-ID 44
     RECT-TABLE AT ROW 3.62 COL 40 WIDGET-ID 46
     RECT-FIELD AT ROW 9.57 COL 40 WIDGET-ID 48
     RECT-QUERYTABLE AT ROW 21 COL 2 WIDGET-ID 54
     RECT-QUERYSORT AT ROW 9.57 COL 82 WIDGET-ID 56
     RECT-QUERYSTR AT ROW 23.14 COL 77 WIDGET-ID 58
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     RECT-COLUMN AT ROW 15.29 COL 71 WIDGET-ID 114
     RECT-PANEL AT ROW 1 COL 78 WIDGET-ID 130
     RECT-JASPER AT ROW 1 COL 130 WIDGET-ID 152
     RECT-SAVE AT ROW 22.67 COL 2 WIDGET-ID 246
     RECT-PARAM AT ROW 9.57 COL 2 WIDGET-ID 260
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.

DEFINE FRAME paramFrame
     btnCloseParam AT ROW 1 COL 6 HELP
          "Jasper Viewer" WIDGET-ID 252
     btnRunResults AT ROW 1 COL 2 HELP
          "Jasper Viewer" WIDGET-ID 254
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 24 ROW 22.67
         SIZE 10 BY 2.38
         FGCOLOR 1  WIDGET-ID 1300.

DEFINE FRAME resultsFrame
     btnCloseResults AT ROW 1 COL 6 HELP
          "Jasper Viewer" WIDGET-ID 252
     btnSaveResults AT ROW 1 COL 2 HELP
          "Jasper Viewer" WIDGET-ID 254
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 13 ROW 22.67
         SIZE 10 BY 2.38
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 1200.


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
         TITLE              = "Dynamic Subject / Query Builder"
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
/* REPARENT FRAME */
ASSIGN FRAME paramFrame:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME resultsFrame:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB subjectBrowse btnMatches DEFAULT-FRAME */
/* BROWSE-TAB tableBrowse subjectBrowse DEFAULT-FRAME */
/* BROWSE-TAB subjectWhereBrowse tableBrowse DEFAULT-FRAME */
/* BROWSE-TAB paramSetBrowse btnGT DEFAULT-FRAME */
/* BROWSE-TAB fieldBrowse paramSetBrowse DEFAULT-FRAME */
/* BROWSE-TAB subjectSortBrowse fieldBrowse DEFAULT-FRAME */
/* BROWSE-TAB subjectTableBrowse btnDec DEFAULT-FRAME */
/* BROWSE-TAB subjectColumnBrowse subjectTableBrowse DEFAULT-FRAME */
/* BROWSE-TAB subjectParamSetBrowse queryStr DEFAULT-FRAME */
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

/* SETTINGS FOR BUTTON btnAddSelections IN FRAME DEFAULT-FRAME
   1 2 3 4 5                                                            */
ASSIGN 
       btnAddSelections:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnAddUseIndex IN FRAME DEFAULT-FRAME
   1 2                                                                  */
ASSIGN 
       btnAddUseIndex:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

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

/* SETTINGS FOR BUTTON btnComma IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnComma:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnCopy IN FRAME DEFAULT-FRAME
   1 6                                                                  */
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

/* SETTINGS FOR BUTTON btnGroupCalc IN FRAME DEFAULT-FRAME
   1 2 3 4 5                                                            */
ASSIGN 
       btnGroupCalc:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnGT IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnGT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

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

/* SETTINGS FOR BUTTON btnRemoveUseIndex IN FRAME DEFAULT-FRAME
   1 2                                                                  */
ASSIGN 
       btnRemoveUseIndex:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnReset IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 6                                                        */
/* SETTINGS FOR BUTTON btnSave IN FRAME DEFAULT-FRAME
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
/* SETTINGS FOR BUTTON btnWhere IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnWhere:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

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

/* SETTINGS FOR COMBO-BOX cParameter IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       cParameter:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN cParameterLabel IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       cParameterLabel:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX cUseIndex IN FRAME DEFAULT-FRAME
   1 2                                                                  */
/* SETTINGS FOR FILL-IN cUseIndexLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 2                                                        */
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

/* SETTINGS FOR BROWSE paramSetBrowse IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       paramSetBrowse:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

/* SETTINGS FOR TOGGLE-BOX paramSetMatches IN FRAME DEFAULT-FRAME
   1 3 4 5                                                              */
ASSIGN 
       paramSetMatches:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN paramSetSearch IN FRAME DEFAULT-FRAME
   ALIGN-L 1 3 4 5                                                      */
ASSIGN 
       paramSetSearch:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR EDITOR queryStr IN FRAME DEFAULT-FRAME
   1 2 3 4                                                              */
ASSIGN 
       queryStr:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE
       queryStr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN queryText IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 2 3 4                                                    */
ASSIGN 
       queryText:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-COLUMN IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 5                                                        */
ASSIGN 
       RECT-COLUMN:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-FIELD IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 3 4 5                                                    */
ASSIGN 
       RECT-FIELD:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-JASPER IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-PANEL IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 6                                                        */
/* SETTINGS FOR RECTANGLE RECT-PARAM IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 3 4 5                                                    */
ASSIGN 
       RECT-PARAM:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

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

/* SETTINGS FOR BROWSE subjectColumnBrowse IN FRAME DEFAULT-FRAME
   1 5                                                                  */
ASSIGN 
       subjectColumnBrowse:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       subjectColumnBrowse:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 1.

/* SETTINGS FOR BROWSE subjectParamSetBrowse IN FRAME DEFAULT-FRAME
   1                                                                    */
ASSIGN 
       subjectParamSetBrowse:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

/* SETTINGS FOR FILL-IN subjectSearch IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR BROWSE subjectSortBrowse IN FRAME DEFAULT-FRAME
   1 4                                                                  */
ASSIGN 
       subjectSortBrowse:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

/* SETTINGS FOR BROWSE subjectTableBrowse IN FRAME DEFAULT-FRAME
   1 2                                                                  */
ASSIGN 
       subjectTableBrowse:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

/* SETTINGS FOR BROWSE subjectWhereBrowse IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       subjectWhereBrowse:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

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

/* SETTINGS FOR FRAME paramFrame
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME paramFrame:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME resultsFrame
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME resultsFrame:HIDDEN           = TRUE.

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

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME paramFrame
/* Query rebuild information for FRAME paramFrame
     _Query            is NOT OPENED
*/  /* FRAME paramFrame */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE paramSetBrowse
/* Query rebuild information for BROWSE paramSetBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH dynParamSet
WHERE (paramSetMatches EQ NO
AND dynParamSet.setName BEGINS paramSetSearch)
OR (paramSetMatches EQ YES
AND dynParamSet.setName MATCHES "*" + paramSetSearch + "*")
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE paramSetBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME resultsFrame
/* Query rebuild information for FRAME resultsFrame
     _Query            is NOT OPENED
*/  /* FRAME resultsFrame */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE subjectBrowse
/* Query rebuild information for BROWSE subjectBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSubject
WHERE (subjectMatches EQ NO  AND ttSubject.subjectName BEGINS subjectSearch)
   OR (subjectMatches EQ YES AND ttSubject.subjectName MATCHES "*" + subjectSearch + "*").
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE subjectBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE subjectColumnBrowse
/* Query rebuild information for BROWSE subjectColumnBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSubjectColumn
WHERE ttSubjectColumn.subjectID EQ ttSubject.subjectID
  AND ((columnMatches EQ NO  AND ttSubjectColumn.fieldName BEGINS columnSearch)
   OR (columnMatches EQ YES AND ttSubjectColumn.fieldName MATCHES "*" + columnSearch + "*"))
BY ttSubjectColumn.sortOrder.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE subjectColumnBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE subjectParamSetBrowse
/* Query rebuild information for BROWSE subjectParamSetBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSubjectParamSet
WHERE ttSubjectParamSet.subjectID EQ ttSubject.subjectID,
FIRST dynParamSet NO-LOCK
WHERE dynParamSet.paramSetID EQ ttSubjectParamSet.paramSetID.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE subjectParamSetBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE subjectSortBrowse
/* Query rebuild information for BROWSE subjectSortBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSubjectSort
WHERE ttSubjectSort.subjectID EQ ttSubject.subjectID
BY ttSubjectSort.sortOrder.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE subjectSortBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE subjectTableBrowse
/* Query rebuild information for BROWSE subjectTableBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSubjectTable
WHERE ttSubjectTable.subjectID EQ ttSubject.subjectID
BY ttSubjectTable.sortOrder.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE subjectTableBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE subjectWhereBrowse
/* Query rebuild information for BROWSE subjectWhereBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSubjectWhere
WHERE ttSubjectWhere.subjectID EQ ttSubject.subjectID
AND ttSubjectWhere.whereTable EQ tableList
BY ttSubjectWhere.sortOrder.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE subjectWhereBrowse */
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
ON END-ERROR OF C-Win /* Dynamic Subject / Query Builder */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Dynamic Subject / Query Builder */
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
ON WINDOW-RESIZED OF C-Win /* Dynamic Subject / Query Builder */
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
    RUN pAddWhere (
        tableList,
        "[[" + ENTRY(2,cParameter,"|") + "]]",
        ENTRY(1,cParameter,"|")
        ).
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


&Scoped-define SELF-NAME btnAddUseIndex
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddUseIndex C-Win
ON CHOOSE OF btnAddUseIndex IN FRAME DEFAULT-FRAME /* Add Use Index */
DO:
    IF AVAILABLE ttSubjectTable THEN DO:
        ASSIGN
            cUseIndex
            ttSubjectTable.useIndex = cUseIndex
            .
        BROWSE subjectTableBrowse:REFRESH().
        fSetSaveButton (YES).
        fShowQuery().
    END.
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


&Scoped-define FRAME-NAME paramFrame
&Scoped-define SELF-NAME btnCloseParam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCloseParam C-Win
ON CHOOSE OF btnCloseParam IN FRAME paramFrame /* Close Results */
DO:
    FRAME paramFrame:HIDDEN = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME resultsFrame
&Scoped-define SELF-NAME btnCloseResults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCloseResults C-Win
ON CHOOSE OF btnCloseResults IN FRAME resultsFrame /* Close Results */
DO:
    IF VALID-HANDLE(hQueryBrowse) THEN
    DELETE OBJECT hQueryBrowse.
    FRAME resultsFrame:HIDDEN = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
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
    RUN pRunSubject (YES, "CSV").
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
    RUN pRunSubject (YES, "DOCX").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGroupCalc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGroupCalc C-Win
ON CHOOSE OF btnGroupCalc IN FRAME DEFAULT-FRAME
DO:
    IF AVAILABLE ttSubjectColumn THEN
    RUN pJasperGroupCalc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnHTML
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHTML C-Win
ON CHOOSE OF btnHTML IN FRAME DEFAULT-FRAME
DO:
    RUN pRunSubject (YES, "HTML").
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
    RUN pRunSubject (YES, "PDF").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRemove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemove C-Win
ON CHOOSE OF btnRemove IN FRAME DEFAULT-FRAME
DO:
    CASE subjectSection:
        WHEN "Columns" THEN
            IF AVAILABLE ttSubjectColumn THEN
            APPLY "DEFAULT-ACTION":U TO subjectColumnBrowse.
        WHEN "Sort" THEN
            IF AVAILABLE ttSubjectSort THEN
            APPLY "DEFAULT-ACTION":U TO subjectSortBrowse.
        WHEN "Table" THEN
            IF AVAILABLE ttSubjectTable THEN
            APPLY "DEFAULT-ACTION":U TO subjectTableBrowse.
        WHEN "Where" THEN
            IF AVAILABLE ttSubjectWhere THEN
            APPLY "DEFAULT-ACTION":U TO subjectWhereBrowse.
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


&Scoped-define SELF-NAME btnRemoveUseIndex
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemoveUseIndex C-Win
ON CHOOSE OF btnRemoveUseIndex IN FRAME DEFAULT-FRAME /* Remove Use Index */
DO:
    IF AVAILABLE ttSubjectTable THEN DO:
        ASSIGN
            cUseIndex
            ttSubjectTable.useIndex = ""
            .
        BROWSE subjectTableBrowse:REFRESH().
        fSetSaveButton (YES).
        fShowQuery().
    END.
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
    RUN pParameterWindow (FRAME paramFrame:HANDLE, "Run").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME paramFrame
&Scoped-define SELF-NAME btnRunResults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRunResults C-Win
ON CHOOSE OF btnRunResults IN FRAME paramFrame /* Save Results */
DO:
    RUN pSaveParamValues.
    RUN pRunSubject (YES, "Results").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* Save */
DO:
    fSetSaveButton (NO).
    RUN pSaveSubject.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME resultsFrame
&Scoped-define SELF-NAME btnSaveResults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSaveResults C-Win
ON CHOOSE OF btnSaveResults IN FRAME resultsFrame /* Save Results */
DO:
    RUN pSaveResults.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnSyntax
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSyntax C-Win
ON CHOOSE OF btnSyntax IN FRAME DEFAULT-FRAME /* Syntax */
DO:
    RUN pRunSubject (NO, ?).
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
    RUN pRunSubject (YES, "View").
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
    RUN pRunSubject (YES, "XLS").
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
    {&OPEN-QUERY-subjectColumnBrowse}
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


&Scoped-define SELF-NAME cUseIndex
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cUseIndex C-Win
ON VALUE-CHANGED OF cUseIndex IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME fieldBrowse
&Scoped-define SELF-NAME fieldBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fieldBrowse C-Win
ON DEFAULT-ACTION OF fieldBrowse IN FRAME DEFAULT-FRAME /* Available Fields */
DO:
    fSetSaveButton (YES).
    CASE subjectSection:
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
    IF AVAILABLE ttSubjectTable AND ttSubjectTable.sortOrder GT 1 THEN DO:
        ttSubjectTable.tableFind = {&SELF-NAME}.
        BROWSE subjectTableBrowse:REFRESH().
        fShowQuery().
    END. /* if avail */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME paramSetBrowse
&Scoped-define SELF-NAME paramSetBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL paramSetBrowse C-Win
ON DEFAULT-ACTION OF paramSetBrowse IN FRAME DEFAULT-FRAME /* Available Parameter Sets */
DO:
    RUN pAddParamSet (dynParamSet.paramSetID).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME paramSetMatches
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL paramSetMatches C-Win
ON VALUE-CHANGED OF paramSetMatches IN FRAME DEFAULT-FRAME /* Matches */
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-paramSetBrowse}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME paramSetSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL paramSetSearch C-Win
ON VALUE-CHANGED OF paramSetSearch IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-paramSetBrowse}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sortType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sortType C-Win
ON VALUE-CHANGED OF sortType IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&SELF-NAME}.
    IF AVAILABLE ttSubjectSort THEN DO:
        ASSIGN
            ttSubjectSort.sortAsc = {&SELF-NAME}
            ttSubjectSort.sortBy  = fSortBy()
            .
        BROWSE subjectSortBrowse:REFRESH().
        fShowQuery().
    END. /* if avail */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME subjectBrowse
&Scoped-define SELF-NAME subjectBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL subjectBrowse C-Win
ON VALUE-CHANGED OF subjectBrowse IN FRAME DEFAULT-FRAME /* Subject */
DO:
    {&OPEN-QUERY-subjectTableBrowse}
    IF AVAILABLE ttSubjectTable THEN
    APPLY "VALUE-CHANGED":U TO BROWSE subjectTableBrowse.
    {&OPEN-QUERY-subjectSortBrowse}
    IF AVAILABLE ttSubjectSort THEN
    APPLY "VALUE-CHANGED":U TO BROWSE subjectSortBrowse.
    {&OPEN-QUERY-subjectColumnBrowse}
    RUN pGetFields.
    {&OPEN-QUERY-paramSetBrowse}
    RUN pGetParamList.
    {&OPEN-QUERY-subjectParamSetBrowse}
    APPLY "VALUE-CHANGED":U TO tableList.
    fShowQuery().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME subjectColumnBrowse
&Scoped-define SELF-NAME subjectColumnBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL subjectColumnBrowse C-Win
ON DEFAULT-ACTION OF subjectColumnBrowse IN FRAME DEFAULT-FRAME /* Subject Columns */
DO:
    DELETE ttSubjectColumn.
    RUN pSetOrder.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME subjectMatches
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL subjectMatches C-Win
ON VALUE-CHANGED OF subjectMatches IN FRAME DEFAULT-FRAME /* Matches */
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-subjectBrowse}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME subjectParamSetBrowse
&Scoped-define SELF-NAME subjectParamSetBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL subjectParamSetBrowse C-Win
ON DEFAULT-ACTION OF subjectParamSetBrowse IN FRAME DEFAULT-FRAME /* Subject Parameter Sets */
DO:
    DELETE ttSubjectParamSet.
    {&OPEN-QUERY-subjectParamSetBrowse}
    RUN pGetParamList.
    fSetSaveButton (YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL subjectParamSetBrowse C-Win
ON VALUE-CHANGED OF subjectParamSetBrowse IN FRAME DEFAULT-FRAME /* Subject Parameter Sets */
DO:
    ASSIGN
        sortType:SCREEN-VALUE = STRING(ttSubjectSort.sortAsc)
        sortType
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME subjectSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL subjectSearch C-Win
ON VALUE-CHANGED OF subjectSearch IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-subjectBrowse}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME subjectSection
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL subjectSection C-Win
ON VALUE-CHANGED OF subjectSection IN FRAME DEFAULT-FRAME
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
        WHEN "Sort" THEN
        VIEW {&sortSection}.
        WHEN "Subject" THEN
        VIEW {&subjectSection}.
        WHEN "Table" THEN
        VIEW {&tableSection}.
        WHEN "Where" THEN
        VIEW {&whereSection}.
    END CASE.
    BROWSE subjectBrowse:WIDTH = IF {&SELF-NAME} NE "Subject" THEN 37
        ELSE FRAME {&FRAME-NAME}:WIDTH - BROWSE subjectBrowse:COL.
    RUN LockWindowUpdate (0,OUTPUT i).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME subjectSortBrowse
&Scoped-define SELF-NAME subjectSortBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL subjectSortBrowse C-Win
ON DEFAULT-ACTION OF subjectSortBrowse IN FRAME DEFAULT-FRAME /* Subject Sort */
DO:
    DELETE ttSubjectSort.
    RUN pSetOrder.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL subjectSortBrowse C-Win
ON VALUE-CHANGED OF subjectSortBrowse IN FRAME DEFAULT-FRAME /* Subject Sort */
DO:
    ASSIGN
        sortType:SCREEN-VALUE = STRING(ttSubjectSort.sortAsc)
        sortType
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME subjectTableBrowse
&Scoped-define SELF-NAME subjectTableBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL subjectTableBrowse C-Win
ON DEFAULT-ACTION OF subjectTableBrowse IN FRAME DEFAULT-FRAME /* Subject Tables */
DO:
    RUN pDeleteSections.
    DELETE ttSubjectTable.
    RUN pGetFields.
    RUN pSetOrder.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL subjectTableBrowse C-Win
ON VALUE-CHANGED OF subjectTableBrowse IN FRAME DEFAULT-FRAME /* Subject Tables */
DO:
    ASSIGN
        findType:SCREEN-VALUE = STRING(ttSubjectTable.tableFind)
        findType
        cUseIndex:LIST-ITEMS = fGetUseIndex(ttSubjectTable.tableDB,ttSubjectTable.tableName)
        cUseIndex:SCREEN-VALUE = cUseIndex:ENTRY(1)
        cUseIndex:INNER-LINES = cUseIndex:NUM-ITEMS
        cUseIndex
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME subjectWhereBrowse
&Scoped-define SELF-NAME subjectWhereBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL subjectWhereBrowse C-Win
ON DEFAULT-ACTION OF subjectWhereBrowse IN FRAME DEFAULT-FRAME /* Subject Where */
DO:
    DELETE ttSubjectWhere.
    RUN pSetOrder.
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
    {&OPEN-QUERY-subjectWhereBrowse}
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

ON "VALUE-CHANGED":U OF ttSubjectColumn.isGroup
DO:
    ttSubjectColumn.isGroup = NOT ttSubjectColumn.isGroup.
    RUN pSetGroupListItems.
END.

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
RUN util/CheckModule.p ("ASI","SubjectBuilder", YES, OUTPUT lContinue).
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
    RUN pLoadSubject.
    RUN pSetObjects.
    RUN pGetSettings.
    RUN enable_UI.
    IF AVAILABLE ttSubject THEN DO:
        RUN pGetDBTables.
        APPLY "VALUE-CHANGED":U TO BROWSE subjectBrowse.
        RUN pGetParamList.
    END. /* if avail */
    RUN LockWindowUpdate (0,OUTPUT i).
  END. /* if continue */
  APPLY "VALUE-CHANGED":U TO subjectSection.
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
  DISPLAY subjectSection subjectSearch subjectMatches tableSearch tableMatches 
          tableList tableListOf paramSetSearch paramSetMatches fieldSearch 
          fieldMatches sortType cUseIndex columnSearch columnMatches cParameter 
          findType cConstant queryStr cUseIndexLabel cParameterLabel 
          cConstantLabel queryText 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnGroupCalc subjectSection btnNow btnToday btnTime btnDateTime 
         subjectSearch subjectMatches tableSearch tableMatches tableList btnOF 
         tableListOf btnWhere btnMatches subjectBrowse tableBrowse 
         subjectWhereBrowse btnBegins btnAND btnOR btnYes btnNo btnEQ btnNE 
         paramSetSearch paramSetMatches fieldSearch fieldMatches sortType btnLT 
         btnGT paramSetBrowse fieldBrowse subjectSortBrowse btnLE btnGE btnPlus 
         btnMinus btnMultiply btnDivide btnDate cUseIndex columnSearch 
         columnMatches btnDec subjectTableBrowse subjectColumnBrowse btnInt 
         btnStr btnSubstr cParameter btnOpen btnClose findType cConstant 
         btnPeriod btnDouble btnComma btnSingle queryStr subjectParamSetBrowse 
         btnResults btnAddUseIndex btnRemoveUseIndex btnAddParameter btnSave 
         btnRemoveSelection btnSyntax btnAddSelections btnMoveDown 
         btnAddConstant btnMoveUp btnRemove btnHTML btnView btnPDF btnDOCX 
         btnCSV btnXLS btnAdd btnCopy btnDelete btnUpdate cParameterLabel 
         cConstantLabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnCloseResults btnSaveResults 
      WITH FRAME resultsFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-resultsFrame}
  ENABLE btnCloseParam btnRunResults 
      WITH FRAME paramFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-paramFrame}
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
    
    DEFINE BUFFER bttSubjectColumn FOR ttSubjectColumn.

    IF CAN-FIND(FIRST ttSubjectColumn
                WHERE ttSubjectColumn.subjectID   EQ ttSubject.subjectID
                  AND ttSubjectColumn.fieldName EQ ipcFieldName) THEN
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
    FOR EACH bttSubjectColumn NO-LOCK
        WHERE bttSubjectColumn.subjectID EQ ttSubject.subjectID
           BY bttSubjectColumn.sortOrder DESCENDING
        :
        LEAVE.
    END. /* for each */
    iOrder = IF AVAILABLE bttSubjectColumn THEN bttSubjectColumn.sortOrder ELSE 0.
    CREATE ttSubjectColumn.
    ASSIGN
        ttSubjectColumn.subjectID     = ttSubject.subjectID
        ttSubjectColumn.sortOrder   = iOrder + 1
        ttSubjectColumn.fieldName   = ipcFieldName
        ttSubjectColumn.fieldLabel  = cLabel
        ttSubjectColumn.fieldFormat = cFormat
        ttSubjectColumn.tableName   = cTable
        ttSubjectColumn.tableDB     = ttField.tableDB
        rRowID                    = ROWID(ttSubjectColumn)
        .
    {&OPEN-QUERY-subjectColumnBrowse}
    REPOSITION subjectColumnBrowse TO ROWID rRowID.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddParamSet C-Win 
PROCEDURE pAddParamSet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiParamSetID AS INTEGER NO-UNDO.
    
    IF CAN-FIND(FIRST ttSubjectParamSet
                WHERE ttSubjectParamSet.subjectID EQ ttSubject.subjectID
                  AND ttSubjectParamSet.paramSetID EQ ipiParamSetID) THEN
    RETURN.
    CREATE ttSubjectParamSet.
    ASSIGN
        ttSubjectParamSet.subjectID  = ttSubject.subjectID
        ttSubjectParamSet.paramSetID = ipiParamSetID
        .
    {&OPEN-QUERY-subjectParamSetBrowse}
    RUN pGetParamList.
    fSetSaveButton (YES).

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
        CASE subjectSection:
            WHEN "Table" THEN DO:
                DO idx = 1 TO tableBrowse:NUM-SELECTED-ROWS:
                    tableBrowse:FETCH-SELECTED-ROW(idx).
                    RUN pAddTable (ttTable.tableDB, ttTable.TableName, ttTable.tableDscr).
                END. /* do idex */
            END. /* table */
            OTHERWISE DO:
                DO idx = 1 TO fieldBrowse:NUM-SELECTED-ROWS:
                    fieldBrowse:FETCH-SELECTED-ROW(idx).
                    CASE subjectSection:
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
    
    DEFINE BUFFER bttSubjectSort FOR ttSubjectSort.

    IF CAN-FIND(FIRST ttSubjectSort
                WHERE ttSubjectSort.subjectID   EQ ttSubject.subjectID
                  AND ttSubjectSort.tableName EQ ENTRY(1,ipcFieldName,".")
                  AND ttSubjectSort.fieldName EQ ENTRY(2,ipcFieldName,".")) THEN
    RETURN.
    FOR EACH bttSubjectSort NO-LOCK
        WHERE bttSubjectSort.subjectID EQ ttSubject.subjectID
           BY bttSubjectSort.sortOrder DESCENDING
        :
        LEAVE.
    END. /* for each */
    iOrder = IF AVAILABLE ttSubjectSort THEN ttSubjectSort.sortOrder ELSE 0.
    CREATE ttSubjectSort.
    ASSIGN
        ttSubjectSort.subjectID    = ttSubject.subjectID
        ttSubjectSort.sortOrder  = iOrder + 1
        ttSubjectSort.tableName  = ENTRY(1,ipcFieldName,".")
        ttSubjectSort.fieldName  = ENTRY(2,ipcFieldName,".")
        ttSubjectSort.fieldLabel = ipcFieldLabel
        ttSubjectSort.sortAsc    = sortType
        ttSubjectSort.sortBy     = fSortBy()
        rRowID                 = ROWID(ttSubjectSort)
        .
    {&OPEN-QUERY-subjectSortBrowse}
    REPOSITION subjectSortBrowse TO ROWID rRowID.

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
    
    DEFINE BUFFER bttSubjectTable FOR ttSubjectTable.

    IF CAN-FIND(FIRST ttSubjectTable
                WHERE ttSubjectTable.subjectID   EQ ttSubject.subjectID
                  AND ttSubjectTable.tableName EQ ipcTableName) THEN
    RETURN NO-APPLY.
    FOR EACH bttSubjectTable NO-LOCK
        WHERE bttSubjectTable.subjectID EQ ttSubject.subjectID
           BY bttSubjectTable.sortOrder DESCENDING
        :
        LEAVE.
    END. /* for each */
    iOrder = IF AVAILABLE bttSubjectTable THEN bttSubjectTable.sortOrder ELSE 0.
    CREATE ttSubjectTable.
    ASSIGN
        ttSubjectTable.subjectID   = ttSubject.subjectID
        ttSubjectTable.sortOrder = iOrder + 1
        ttSubjectTable.tableName = ipcTableName
        ttSubjectTable.tableDscr = ipcTableDscr
        ttSubjectTable.tableFind = findType
        ttSubjectTable.tableDB   = ipcTableDB
        rRowID                 = ROWID(ttSubjectTable)
        .
    {&OPEN-QUERY-subjectTableBrowse}
    REPOSITION subjectTableBrowse TO ROWID rRowID.
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
    
    DEFINE BUFFER bttSubjectWhere FOR ttSubjectWhere.
    
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
    FOR EACH bttSubjectWhere NO-LOCK
        WHERE bttSubjectWhere.subjectID    EQ ttSubject.subjectID
          AND bttSubjectWhere.whereTable EQ tableList
           BY bttSubjectWhere.sortOrder DESCENDING
        :
        LEAVE.
    END. /* for each */
    iOrder = IF AVAILABLE bttSubjectWhere THEN bttSubjectWhere.sortOrder ELSE 0.
    CREATE ttSubjectWhere.
    ASSIGN
        ttSubjectWhere.subjectID      = ttSubject.subjectID
        ttSubjectWhere.sortOrder    = iOrder + 1
        ttSubjectWhere.tableName    = cTable
        ttSubjectWhere.whereTable   = ipcTableName
        ttSubjectWhere.whereElement = ipcElement
        ttSubjectWhere.fieldLabel   = ipcTableLabel
        ttSubjectWhere.dataType     = cDataType
        rRowID                    = ROWID(ttSubjectWhere)
        .
    {&OPEN-QUERY-subjectWhereBrowse}
    REPOSITION subjectWhereBrowse TO ROWID rRowID.
    fSetSaveButton (YES).
    fShowQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCalendar C-Win 
PROCEDURE pCalendar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE calendarDate AS CHARACTER NO-UNDO.
    
    RUN nosweat/popupcal.w (OUTPUT calendarDate).
    IF calendarDate NE '' THEN
    iphWidget:SCREEN-VALUE = calendarDate.
    RETURN NO-APPLY.    

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDatePickList C-Win 
PROCEDURE pDatePickList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget   AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER iphCalendar AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER iphPickList AS HANDLE NO-UNDO.
    
    ASSIGN
        iphWidget:SCREEN-VALUE = STRING(fDateOptionValue(
            iphPickList:SCREEN-VALUE,
            DATE(iphWidget:SCREEN-VALUE)
            ))
        iphWidget:READ-ONLY = iphPickList:SCREEN-VALUE NE "Fixed Date"
        iphWidget:PRIVATE-DATA = iphPickList:SCREEN-VALUE
        .
    IF VALID-HANDLE(iphCalendar) THEN
    iphCalendar:SENSITIVE = iphPickList:SCREEN-VALUE EQ "Fixed Date".

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
    IF CAN-FIND(FIRST ttSubjectColumn
            WHERE ttSubjectColumn.subjectID   EQ ttSubject.subjectID
              AND ttSubjectColumn.tableName EQ ttSubjectTable.tableName) THEN DO:
        FOR EACH ttSubjectColumn
            WHERE ttSubjectColumn.subjectID   EQ ttSubject.subjectID
              AND ttSubjectColumn.tableName EQ ttSubjectTable.tableName
            :
            DELETE ttSubjectColumn.
        END. /* each ttSubjectSort */
        subjectSection = "Column".
        RUN pSetOrder.
    END. /* if can-find */
    IF CAN-FIND(FIRST ttSubjectSort
            WHERE ttSubjectSort.subjectID   EQ ttSubject.subjectID
              AND ttSubjectSort.tableName EQ ttSubjectTable.tableName) THEN DO:
        FOR EACH ttSubjectSort
            WHERE ttSubjectSort.subjectID   EQ ttSubject.subjectID
              AND ttSubjectSort.tableName EQ ttSubjectTable.tableName
            :
            DELETE ttSubjectSort.
        END. /* each ttSubjectSort */
        subjectSection = "Sort".
        RUN pSetOrder.
    END. /* if can-find */
    IF CAN-FIND(FIRST ttSubjectWhere
            WHERE ttSubjectWhere.subjectID   EQ ttSubject.subjectID
              AND ttSubjectWhere.tableName EQ ttSubjectTable.tableName) THEN DO:
        FOR EACH ttSubjectWhere
            WHERE ttSubjectWhere.subjectID   EQ ttSubject.subjectID
              AND ttSubjectWhere.tableName EQ ttSubjectTable.tableName
            :
            DELETE ttSubjectWhere.
        END. /* each ttSubjectWhere */
        subjectSection = "Where".
        RUN pSetOrder.
    END. /* if can-find */
    subjectSection = "Table".

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

    DEFINE BUFFER ttSubjectTable FOR ttSubjectTable.
    
    EMPTY TEMP-TABLE ttField.
    ASSIGN
        tableList:LIST-ITEMS IN FRAME {&FRAME-NAME} = ?
        tableListOf:LIST-ITEMS = ?
        .
    FOR EACH ttSubjectTable
        WHERE ttSubjectTable.subjectID EQ ttSubject.subjectID
        :
        tableList:ADD-LAST(ttSubjectTable.tableName).
        tableListOf:ADD-LAST(ttSubjectTable.tableName).
        CREATE ALIAS "dictdb" FOR DATABASE VALUE(ttSubjectTable.tableDB).
        RUN nosweat/fld_list.p (ttSubjectTable.tableName, OUTPUT cFieldList).
        DO idx = 1 TO NUM-ENTRIES(cFieldList):
            cField = ENTRY(idx,cFieldList).
            IF INDEX(cField,"[") NE 0 THEN
            cField = SUBSTR(cField,1,INDEX(cField,"[") - 1).
            RUN nosweat/fld_lbls.p (ttSubjectTable.tableName, cField, OUTPUT cLabel).
            IF cLabel EQ "" THEN
            cLabel = ENTRY(idx,cFieldList).
            CREATE ttField.
            ASSIGN
                ttField.tableDB    = ttSubjectTable.tableDB
                ttField.fieldLabel = cLabel
                ttField.fieldName  = ttSubjectTable.tableName + "."
                                   + ENTRY(idx,cFieldList)
                                   .
        END. /* do idx */
    END. /* each ttSubjectTable */
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
    DEFINE VARIABLE cParamLabel AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParamName  AS CHARACTER NO-UNDO.
    
    cParameter:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = ?.
    FOR EACH ttSubjectParamSet NO-LOCK
        WHERE ttSubjectParamSet.subjectID EQ ttSubject.subjectID,
        EACH dynParamSetDtl NO-LOCK
        WHERE dynParamSetDtl.paramSetID EQ ttSubjectParamSet.paramSetID,
        FIRST dynParam OF dynParamSetDtl NO-LOCK
        :
        ASSIGN
            cParamLabel = IF dynParamSetDtl.paramLabel NE "" THEN dynParamSetDtl.paramLabel
                          ELSE dynParam.paramLabel
            cParamName  = IF dynParamSetDtl.paramName NE "" THEN dynParamSetDtl.paramName
                          ELSE dynParam.paramName
                          .
        cParameter:ADD-LAST(cParamLabel + " (" + cParamName + ")",
                            cParamLabel + "|" + cParamName).
    END. /* each dynparamset */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperGroupCalc C-Win 
PROCEDURE pJasperGroupCalc :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cGroupCalc AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSave      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bttSubjectColumn FOR ttSubjectColumn.
    
    FOR EACH ttGroupCalc
        WHERE ttGroupCalc.subjectID EQ ttSubjectColumn.subjectID
          AND ttGroupCalc.fieldName EQ ttSubjectColumn.fieldName
        :
        cGroupCalc = cGroupCalc
                   + ttGroupCalc.groupName + ","
                   + ttGroupCalc.calcType + ","
                   .
    END. /* each ttgroupcalc */
    cGroupCalc = TRIM(cGroupCalc,",").
    RUN AOA/jasperGroupCalc.w (
        ttSubjectColumn.fieldLabe,
        ttSubjectColumn.fieldName,
        fJasperGroups(),
        fJasperFields(),
        fJasperVariables(),
        INPUT-OUTPUT cGroupCalc,
        OUTPUT lSave
        ).
    IF lSave THEN DO:
        FOR EACH ttGroupCalc
            WHERE ttGroupCalc.subjectID EQ ttSubjectColumn.subjectID
              AND ttGroupCalc.fieldName EQ ttSubjectColumn.fieldName
            :
            DELETE ttGroupCalc.
        END. /* each ttgroupcalc */
        IF cGroupCalc NE "" THEN
        DO idx = 1 TO NUM-ENTRIES(cGroupCalc) BY 2:
            CREATE ttGroupCalc.
            ASSIGN
                ttGroupCalc.subjectID = ttSubjectColumn.subjectID
                ttGroupCalc.fieldName = ttSubjectColumn.fieldName
                ttGroupCalc.groupName = ENTRY(idx,cGroupCalc)
                ttGroupCalc.calcType  = ENTRY(idx + 1,cGroupCalc)
                .
        END. /* do idx */
        ttSubjectColumn.groupCalc = fJasperGroupCalc(ttSubjectColumn.fieldName).
        BROWSE subjectColumnBrowse:REFRESH() NO-ERROR.
    END. /* if lsave */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLoadSubject C-Win 
PROCEDURE pLoadSubject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bttSubjectWhere     FOR ttSubjectWhere.
    DEFINE BUFFER bttSubjectSort      FOR ttSubjectSort.
    DEFINE BUFFER bttSubjectColumn    FOR ttSubjectColumn.
    
    FOR EACH dynSubject NO-LOCK:
        CREATE ttSubject.
        BUFFER-COPY dynSubject TO ttSubject.
    END. /* for each */

    FOR EACH dynSubjectTable:
        CREATE ttSubjectTable.
        BUFFER-COPY dynSubjectTable TO ttSubjectTable.
    END. /* for each */

    FOR EACH dynSubjectWhere:
        CREATE ttSubjectWhere.
        BUFFER-COPY dynSubjectWhere TO ttSubjectWhere.
    END. /* for each */

    FOR EACH dynSubjectSort NO-LOCK:
        CREATE ttSubjectSort.
        BUFFER-COPY dynSubjectSort TO ttSubjectSort.
    END. /* for each */

    FOR EACH dynSubjectColumn NO-LOCK:
        CREATE ttSubjectColumn.
        BUFFER-COPY dynSubjectColumn TO ttSubjectColumn.
    END. /* for each */

    FOR EACH dynSubjectParamSet:
        CREATE ttSubjectParamSet.
        BUFFER-COPY dynSubjectParamSet TO ttSubjectParamSet.
    END. /* for each */

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
    
    DEFINE BUFFER bttSubjectColumn FOR ttSubjectColumn.
    DEFINE BUFFER bttSubjectSort   FOR ttSubjectSort.
    DEFINE BUFFER bttSubjectTable  FOR ttSubjectTable.
    DEFINE BUFFER bttSubjectWhere  FOR ttSubjectWhere.
    
    CASE subjectSection:
        WHEN "Columns" THEN DO:
            {AOA/includes/pMove.i "ttSubjectColumn" "subjectColumnBrowse"}
        END. /* columns */
        WHEN "Sort" THEN DO:
            {AOA/includes/pMove.i "ttSubjectSort" "subjectSortBrowse"}
        END. /* sort */
        WHEN "Table" THEN DO:
            {AOA/includes/pMove.i "ttSubjectTable" "subjectTableBrowse"}
        END. /* table */
        WHEN "Where" THEN DO:
            {AOA/includes/pMove.i "ttSubjectWhere" "subjectWhereBrowse"}
        END. /* where */
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pParamAction C-Win 
PROCEDURE pParamAction :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE cAction  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iParamID AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bttAction FOR ttAction.
    
    FIND FIRST ttAction
         WHERE ttACtion.paramWidget EQ iphWidget
         NO-ERROR.
    IF NOT AVAILABLE ttAction THEN RETURN.
    iParamID = ttAction.paramID.
    FOR EACH ttAction
        WHERE ttAction.actionParamID EQ iParamID
          AND ttAction.action NE ""
        :
        DO idx = 1 TO NUM-ENTRIES(ttAction.action):
            ASSIGN
                cValue  = ENTRY(1,ENTRY(idx,ttAction.action),":")
                cAction = ENTRY(2,ENTRY(idx,ttAction.action),":")
                .
            IF iphWidget:SCREEN-VALUE EQ cValue THEN
            FOR EACH bttAction
                WHERE bttAction.paramID EQ ttAction.paramID,
                FIRST dynParam
                WHERE dynParam.paramID EQ bttAction.paramID
                :
                CASE cAction:
                    WHEN "DISABLE" THEN
                    bttACtion.paramWidget:READ-ONLY = YES.
                    WHEN "ENABLE" THEN
                    bttACtion.paramWidget:READ-ONLY = NO.
                    WHEN "HI" THEN
                    CASE dynParam.dataType:
                        WHEN "CHARACTER" THEN
                        bttACtion.paramWidget:SCREEN-VALUE = CHR(254).
                        WHEN "DECIMAL" THEN
                        bttACtion.paramWidget:SCREEN-VALUE = "99999999.99".
                        WHEN "DATE" THEN
                        bttACtion.paramWidget:SCREEN-VALUE = "12/31/2049".
                        WHEN "INTEGER" THEN
                        bttACtion.paramWidget:SCREEN-VALUE = "99999999".
                    END CASE.
                    WHEN "LOW" THEN
                    CASE dynParam.dataType:
                        WHEN "CHARACTER" THEN
                        bttACtion.paramWidget:SCREEN-VALUE = CHR(32).
                        WHEN "DECIMAL" THEN
                        bttACtion.paramWidget:SCREEN-VALUE = "0".
                        WHEN "DATE" THEN
                        bttACtion.paramWidget:SCREEN-VALUE = "1/1/1950".
                        WHEN "INTEGER" THEN
                        bttACtion.paramWidget:SCREEN-VALUE = "0".
                    END CASE.
                END CASE.
            END. /* each bttaction */
        END. /* do idx */
    END. /* each ttaction */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pParameterWindow C-Win 
PROCEDURE pParameterWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphFrame AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER ipcMode  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cParamName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParamLabel AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoolName   AS CHARACTER NO-UNDO INITIAL "ParameterPool".
    DEFINE VARIABLE dCol        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dRow        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE hCalendar   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hLabel      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hPickList   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hWidget     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE kdx         AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER ttSubjectParamSet FOR ttSubjectParamSet.
    
    DELETE WIDGET-POOL "parameterPool" NO-ERROR.
    CREATE WIDGET-POOL "parameterPool" PERSISTENT.
    
    IF NOT CAN-FIND(FIRST ttSubjectParamSet
                    WHERE ttSubjectParamSet.subjectID EQ ttSubject.subjectID) THEN DO:
        IF ipcMode EQ "Run" THEN
        RUN pRunSubject (YES, "Results").
        RETURN.
    END.
    
    EMPTY TEMP-TABLE ttAction.
    
    FOR EACH ttSubjectParamSet
        WHERE ttSubjectParamSet.subjectID EQ ttSubject.subjectID,
        EACH dynParamSet NO-LOCK
        WHERE dynParamSet.paramSetID EQ ttSubjectParamSet.paramSetID,
        EACH dynParamSetDtl
        WHERE dynParamSetDtl.paramSetID EQ dynParamSet.paramSetID,
        FIRST dynParam NO-LOCK
        WHERE dynParam.paramID EQ dynParamSetDtl.paramID
        :
        ASSIGN
            cParamName  = IF dynParamSetDtl.paramName  NE "" THEN dynParamSetDtl.paramName
                          ELSE dynParam.paramName
            cParamLabel = IF dynParamSetDtl.paramLabel NE "" THEN dynParamSetDtl.paramLabel
                          ELSE dynParam.paramLabel
            dCol        = dynParamSetDtl.paramCol + ttSubjectParamSet.setCol - 1
            dRow        = dynParamSetDtl.paramRow + ttSubjectParamSet.setRow - 1
            .
        IF dynParamSet.setRectangle THEN DO:
            idx = idx + 1.
            CREATE RECTANGLE hWidget IN WIDGET-POOL cPoolName
                ASSIGN
                    FRAME = iphFrame
                    NAME = "RECT-" + STRING(idx)
                    GRAPHIC-EDGE = YES
                    ROUNDED = YES
                    WIDTH-PIXELS = 1
                    COL = ttSubjectParamSet.setCol + 1
                    ROW = ttSubjectParamSet.setRow + .48
                    SENSITIVE = NO
                    WIDTH = dynParamSet.setWidth - 2.2
                    HEIGHT = dynParamSet.setHeight - .76
                    FILLED = NO
                    .
            IF dynParamSet.setTitle NE "" THEN
            CREATE TEXT hLabel IN WIDGET-POOL cPoolName
              ASSIGN
                FRAME = iphFrame
                AUTO-RESIZE = YES
                HEIGHT = .62
                WIDTH = FONT-TABLE:GET-TEXT-WIDTH-CHARS(" " + dynParamSet.setTitle,iphFrame:FONT) + 1
                COL = hWidget:COL + 2
                ROW = hWidget:ROW - .24
                FORMAT = "x(" + STRING(LENGTH(" " + dynParamSet.setTitle)) + ")"
                SCREEN-VALUE = " " + dynParamSet.setTitle
                SENSITIVE = YES
                .
        END. /* if rectangle */
        CASE dynParam.viewAs:
            WHEN "COMBO-BOX" THEN DO:
                hLabel = fCreateLabel(cPoolName, iphFrame, cParamLabel, dRow).
                CREATE COMBO-BOX hWidget IN WIDGET-POOL cPoolName
                  ASSIGN
                    FRAME = iphFrame
                    NAME = cParamName
                    COL = dCol + 2
                    ROW = dRow
                    WIDTH = dynParam.paramWidth
                    LIST-ITEMS = dynParamSetDtl.initialItems
                    FORMAT = dynParam.paramFormat
                    SCREEN-VALUE = dynParamSetDtl.initialValue
                    INNER-LINES = dynParam.innerLines
                    SIDE-LABEL-HANDLE = hLabel
                    SENSITIVE = dynParamSetDtl.paramPrompt
                TRIGGERS:
                  ON VALUE-CHANGED
                    PERSISTENT RUN pParamAction IN THIS-PROCEDURE (hWidget:HANDLE).
                END TRIGGERS.
                hLabel:COL = hWidget:COL - hLabel:WIDTH.
            END. /* combo-box */
            WHEN "EDITOR" THEN DO:
                CREATE EDITOR hWidget IN WIDGET-POOL cPoolName
                    ASSIGN
                        FRAME = iphFrame
                        NAME = dynParamSetDtl.paramName
                        COL = dCol
                        ROW = dRow
                        WIDTH = dynParam.paramWidth
                        HEIGHT = dynParam.paramHeight
                        SCROLLBAR-HORIZONTAL = CAN-DO(dynParamSetDtl.action,"HORIZONTAL")
                        SCROLLBAR-VERTICAL = CAN-DO(dynParamSetDtl.action,"VERTICAL")
                        WORD-WRAP = NO
                        BOX = YES
                        SCREEN-VALUE = dynParamSetDtl.initialValue
                        SENSITIVE = YES
                        READ-ONLY = dynParamSetDtl.paramPrompt EQ NO
                    TRIGGERS:
                      ON LEAVE
                        PERSISTENT RUN pValidate IN THIS-PROCEDURE (hWidget:HANDLE).
                    END TRIGGERS.
                ASSIGN
                    hLabel = fCreateLabel(cPoolName, iphFrame, cParamLabel, dRow)
                    hLabel:COL = dCol - hLabel:WIDTH
                    .
                IF CAN-DO(dynParamSetDtl.action,"EMAIL") THEN DO:
                    ASSIGN
                        dCol = dCol - 5
                        dRow = dRow + .95
                        .
                    CREATE BUTTON hCalendar IN WIDGET-POOL cPoolName
                        ASSIGN
                            FRAME = iphFrame
                            NAME = "btnRecipients"
                            COL = dCol
                            ROW = dRow
                            WIDTH = 4.4
                            HEIGHT = 1.05
                            TOOLTIP = "Add Recipients"
                            SENSITIVE = dynParamSetDtl.paramPrompt
                        TRIGGERS:
                          ON CHOOSE
                            PERSISTENT RUN pRecipients IN THIS-PROCEDURE (hWidget:HANDLE).
                        END TRIGGERS.
                    IF VALID-HANDLE(hCalendar) THEN
                    hCalendar:LOAD-IMAGE("AOA\images\navigate_plus.gif").
                END. /* if use a calendar */
            END. /* editor */
            WHEN "FILL-IN" THEN DO:
                hLabel = fCreateLabel(cPoolName, iphFrame, cParamLabel, dRow).
                CREATE FILL-IN hWidget IN WIDGET-POOL cPoolName
                    ASSIGN
                        FRAME = iphFrame
                        NAME = cParamName
                        DATA-TYPE = dynParam.dataType
                        FORMAT = dynParam.paramFormat
                        COL = dCol + 2
                        ROW = dRow
                        WIDTH = dynParam.paramWidth
                        HEIGHT = dynParam.paramHeight
                        SCREEN-VALUE = dynParamSetDtl.initialValue
                        SIDE-LABEL-HANDLE = hLabel
                        SENSITIVE = YES
                        READ-ONLY = dynParamSetDtl.paramPrompt EQ NO
                    TRIGGERS:
                      ON LEAVE
                        PERSISTENT RUN pValidate IN THIS-PROCEDURE (hWidget:HANDLE).
                    END TRIGGERS.
                ASSIGN
                    hLabel:COL = hWidget:COL - hLabel:WIDTH
                    dCol = hWidget:COL + hWidget:WIDTH + .4
                    hCalendar = ?
                    .
                IF dynParam.dataType EQ "DATE" THEN DO:
                    IF CAN-DO(dynParamSetDtl.action,"CALENDAR") THEN DO:
                        jdx  = jdx + 1.
                        CREATE BUTTON hCalendar IN WIDGET-POOL cPoolName
                            ASSIGN
                                FRAME = iphFrame
                                NAME = "btnCalendar-" + STRING(jdx)
                                COL = dCol
                                ROW = dRow
                                WIDTH = 4.6
                                HEIGHT = 1.05
                                TOOLTIP = "Calendar Popup"
                                SENSITIVE = dynParamSetDtl.paramPrompt
                            TRIGGERS:
                              ON CHOOSE
                                PERSISTENT RUN pCalendar IN THIS-PROCEDURE (hWidget:HANDLE).
                            END TRIGGERS.
                        IF VALID-HANDLE(hCalendar) THEN DO:
                            hCalendar:LOAD-IMAGE("Graphics\16x16\calendar.bmp").
                            dCol = hCalendar:COL + hCalendar:WIDTH + .4.
                        END. /* if valid-handle */
                    END. /* if use a calendar */
                    IF CAN-DO(dynParamSetDtl.action,"DATEPICKLIST") THEN DO:
                        kdx = kdx + 1.
                        CREATE COMBO-BOX hPickList IN WIDGET-POOL cPoolName
                          ASSIGN
                            FRAME = iphFrame
                            NAME = "DatePickList-" + STRING(kdx)
                            FORMAT = "x(256)"
                            COL = dCol
                            ROW = dRow
                            WIDTH = 25
                            TOOLTIP = "Date Pick List"
                            PRIVATE-DATA = hWidget:NAME
                            SENSITIVE = dynParamSetDtl.paramPrompt
                        TRIGGERS:
                          ON VALUE-CHANGED
                            PERSISTENT RUN pDatePickList IN THIS-PROCEDURE (
                                hWidget:HANDLE,
                                hCalendar:HANDLE,
                                hPickList:HANDLE
                                ).
                        END TRIGGERS.
                        fDateOptions(hPickList).
                        ASSIGN
                            hPickList:SCREEN-VALUE = hPickList:ENTRY(1)
                            hWidget:PRIVATE-DATA = hPickList:SCREEN-VALUE
                            .
                    END. /* if date pick list */
                END. /* if date type */
            END. /* fill-in */
            WHEN "RADIO-SET" THEN DO:
                CREATE RADIO-SET hWidget IN WIDGET-POOL cPoolName
                    ASSIGN
                        FRAME = iphFrame
                        NAME = cParamName
                        RADIO-BUTTONS = dynParamSetDtl.initialItems
                        HORIZONTAL = CAN-DO(dynParamSetDtl.action,"HORIZONTAL")
                        COL = dCol
                        ROW = dRow
                        WIDTH = dynParam.paramWidth
                        HEIGHT = dynParam.paramHeight
                        SCREEN-VALUE = dynParamSetDtl.initialValue
                        SENSITIVE = dynParamSetDtl.paramPrompt
                    TRIGGERS:
                      ON VALUE-CHANGED
                        PERSISTENT RUN pParamAction IN THIS-PROCEDURE (hWidget:HANDLE).
                    END TRIGGERS.
                ASSIGN
                    hLabel = fCreateLabel(cPoolName, iphFrame, cParamLabel, dRow)
                    hLabel:COL = dCol - hLabel:WIDTH
                    .
            END. /* radio-set */
            WHEN "SELECTION-LIST" THEN DO:
                CREATE SELECTION-LIST hWidget IN WIDGET-POOL cPoolName
                  ASSIGN
                    FRAME = iphFrame
                    NAME = cParamName
                    COL = dCol
                    ROW = dRow
                    WIDTH = dynParam.paramWidth
                    SCROLLBAR-VERTICAL = YES
                    MULTIPLE = CAN-DO(dynParamSetDtl.action,"MULTISELECT")
                    LIST-ITEMS = dynParamSetDtl.initialItems
                    SCREEN-VALUE = dynParamSetDtl.initialValue
                    INNER-LINES = dynParam.innerLines
                    SENSITIVE = dynParamSetDtl.paramPrompt
                TRIGGERS:
                  ON VALUE-CHANGED
                    PERSISTENT RUN pParamAction IN THIS-PROCEDURE (hWidget:HANDLE).
                END TRIGGERS.
                ASSIGN
                    hLabel = fCreateLabel(cPoolName, iphFrame, cParamLabel, dRow)
                    hLabel:COL = dCol - hLabel:WIDTH
                    .
            END. /* selection-list */
            WHEN "TOGGLE-BOX" THEN DO:
                CREATE TOGGLE-BOX hWidget IN WIDGET-POOL cPoolName
                    ASSIGN
                        FRAME = iphFrame
                        NAME = cParamName
                        LABEL = cParamLabel
                        COL = dCol
                        ROW = dRow
                        WIDTH = dynParam.paramWidth
                        HEIGHT = dynParam.paramHeight
                        SCREEN-VALUE = dynParamSetDtl.initialValue
                        SENSITIVE = dynParamSetDtl.paramPrompt
                    TRIGGERS:
                      ON VALUE-CHANGED
                        PERSISTENT RUN pParamAction IN THIS-PROCEDURE (hWidget:HANDLE).
                    END TRIGGERS.
            END. /* toggle-box */
        END CASE.
        IF VALID-HANDLE(hWidget) THEN DO:
            CREATE ttAction.
            ASSIGN
                ttAction.paramWidget    = hWidget:HANDLE
                ttAction.paramID        = dynParamSetDtl.paramID
                ttAction.actionParamID  = dynParamSetDtl.actionParamID
                ttAction.action         = dynParamSetDtl.action
                ttAction.initializeProc = dynParam.initializeProc
                ttAction.validateProc   = dynParam.validateProc
                .
        END. /* if valid-handle */
    END. /* each ttSubjectParamSet */
    iphFrame:HIDDEN = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRecipients C-Win 
PROCEDURE pRecipients :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE cRecipients AS CHARACTER NO-UNDO.
    
    cRecipients = iphWidget:SCREEN-VALUE.
    RUN AOA/aoaRecipients.w (INPUT-OUTPUT cRecipients).
    iphWidget:SCREEN-VALUE = cRecipients.

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
        CASE subjectSection:
            WHEN "Columns" THEN DO:
                DO idx = 1 TO subjectColumnBrowse:NUM-SELECTED-ROWS:
                    subjectColumnBrowse:FETCH-SELECTED-ROW(idx).
                    DELETE ttSubjectColumn.
                END. /* do idex */
            END. /* columns */
            WHEN "Sort" THEN DO:
                DO idx = 1 TO subjectSortBrowse:NUM-SELECTED-ROWS:
                    subjectSortBrowse:FETCH-SELECTED-ROW(idx).
                    DELETE ttSubjectSort.
                END. /* do idex */
            END. /* sort */
            WHEN "Table" THEN DO:
                DO idx = 1 TO subjectTableBrowse:NUM-SELECTED-ROWS:
                    subjectTableBrowse:FETCH-SELECTED-ROW(idx).
                    RUN pDeleteSections.
                    DELETE ttSubjectTable.
                END. /* do idex */
                RUN pGetFields.
            END. /* table */
            WHEN "Where" THEN DO:
                DO idx = 1 TO subjectWhereBrowse:NUM-SELECTED-ROWS:
                    subjectWhereBrowse:FETCH-SELECTED-ROW(idx).
                    DELETE ttSubjectWhere.
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
    
    DEFINE BUFFER bttSubjectColumn FOR ttSubjectColumn.
    
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    IF VALID-HANDLE(hQueryBrowse) THEN
    DELETE OBJECT hQueryBrowse.
    
    iphQuery:QUERY-OPEN.
    CREATE BROWSE hQueryBrowse
        ASSIGN
            FRAME = FRAME resultsFrame:HANDLE
            TITLE = "Subject Query Results"
            SENSITIVE = TRUE
            SEPARATORS = TRUE
            ROW-MARKERS = FALSE
            COLUMN-RESIZABLE = TRUE
            COLUMN-MOVABLE = TRUE 
            ALLOW-COLUMN-SEARCHING = TRUE
            QUERY = iphQuery
            COL = 1
            ROW = 1
            HEIGHT = FRAME resultsFrame:HEIGHT - .1
            WIDTH = FRAME resultsFrame:WIDTH - .32
            VISIBLE = TRUE
            NO-VALIDATE = TRUE
            .
    FOR EACH bttSubjectColumn
        WHERE bttSubjectColumn.subjectID EQ ttSubject.subjectID
           BY bttSubjectColumn.sortOrder
        :
        hColumn = hQueryBrowse:ADD-LIKE-COLUMN(bttSubjectColumn.fieldName).
        /*
        IF bttSubjectColumn.sortOrder MOD 2 EQ 0 THEN
        hColumn:COLUMN-BGCOLOR = 11.
        */
        IF bttSubjectColumn.columnSize NE 0 THEN
        hColumn:WIDTH-CHARS = bttSubjectColumn.columnSize.
    END. /* each ttSubjectColumn */
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
    
    DEFINE BUFFER bttSubjectTable  FOR ttSubjectTable.
    DEFINE BUFFER bttSubjectColumn FOR ttSubjectColumn.
    
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    
    iphQuery:QUERY-OPEN.
    iphQuery:GET-FIRST().
    IF NOT iphQuery:QUERY-OFF-END THEN DO:
        RUN pEmptyttColumn IN hJasper.
        FOR EACH bttSubjectColumn
            WHERE bttSubjectColumn.subjectID EQ ttSubject.subjectID
            :
            ASSIGN
                hQueryBuf    = iphQuery:GET-BUFFER-HANDLE(bttSubjectColumn.tableName)
                cFieldName   = ENTRY(2,bttSubjectColumn.fieldName,".")
                .
            RUN pCreatettColumn IN hJasper (
                cFieldName,
                bttSubjectColumn.sortOrder,
                YES,
                bttSubjectColumn.fieldLabel,
                bttSubjectColumn.dataType,
                bttSubjectColumn.fieldFormat,
                hQueryBuf:BUFFER-FIELD(cFieldName):WIDTH,
                MAX(hQueryBuf:BUFFER-FIELD(cFieldName):WIDTH,LENGTH(hQueryBuf:BUFFER-FIELD(cFieldName):LABEL))
                ).
        END. /* do iColumn */
        OS-CREATE-DIR "users".
        OS-CREATE-DIR VALUE("users\" + USERID("ASI")).
        OS-CREATE-DIR VALUE("users\" + USERID("ASI") + "\Jasper").
        cJasperFile = "users\" + USERID("ASI") + "\"
                    + REPLACE(ttSubject.subjectName," ","")
                    + ".json"
                    .
        OUTPUT TO VALUE(cJasperFile).
        PUT UNFORMATTED
            "~{" SKIP
            FILL(" ",2)
            "~"" REPLACE(ttSubject.subjectName," ","_") "~": ~{" SKIP
            FILL(" ",4)
            "~"tt" REPLACE(ttSubject.subjectName," ","") "~": [" SKIP
            .
        REPEAT:
            PUT UNFORMATTED
                FILL(" ",6) "~{" SKIP
                .
            FOR EACH bttSubjectColumn
                WHERE bttSubjectColumn.subjectID EQ ttSubject.subjectID
                   BY bttSubjectColumn.sortOrder
                :
                ASSIGN
                    hQueryBuf    = iphQuery:GET-BUFFER-HANDLE(bttSubjectColumn.tableName)
                    cFieldName   = ENTRY(2,bttSubjectColumn.fieldName,".")
                    cBufferValue = fFormatValue(hQueryBuf, hQueryBuf:BUFFER-FIELD(cFieldName):NAME)
                    /* remove special characters with escape values */
                    cBufferValue = REPLACE(cBufferValue,"~&","~&amp;")
                    cBufferValue = REPLACE(cBufferValue,"~'","~&apos;")
                    cBufferValue = REPLACE(cBufferValue,"~"","~&quot;")
                    cBufferValue = REPLACE(cBufferValue,"<","~&lt;")
                    cBufferValue = REPLACE(cBufferValue,">","~&gt;")
                    cBufferValue = REPLACE(cBufferValue,"~\","~\~\")
                    .
                IF bttSubjectColumn.sortOrder GT 1 THEN
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
        RUN pJasperCopy IN hJasper (cJasperFile).    
        RUN spJasperQuery IN hJasper (
            ipcType,
            ttSubject.subjectName,
            USERID("ASI"),
            OUTPUT cJasperFile
            ).
    END. /* if get-first is valid */
    iphQuery:QUERY-CLOSE().
    
    RUN LockWindowUpdate (0,OUTPUT i).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunSubject C-Win 
PROCEDURE pRunSubject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplRun  AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cDate     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cError    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQueryStr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParam    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtDate    AS DATE      NO-UNDO.
    DEFINE VARIABLE hBuffer   AS HANDLE    NO-UNDO EXTENT 1000.
    DEFINE VARIABLE hQuery    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lOK       AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER ttSubjectParamSet FOR ttSubjectParamSet.
    DEFINE BUFFER ttSubjectTable    FOR ttSubjectTable.
    
    DO TRANSACTION:
        FIND FIRST dynParamValue EXCLUSIVE-LOCK
             WHERE dynParamValue.subjectID    EQ ttSubject.subjectID
               AND dynParamValue.user-id      EQ "_default"
               AND dynParamValue.prgmName     EQ "{&program-id}"
               AND dynParamValue.paramValueID EQ 0
             NO-ERROR.
        IF NOT AVAILABLE dynParamValue THEN DO:
            CREATE dynParamValue.
            ASSIGN
                dynParamValue.subjectID        = ttSubject.subjectID
                dynParamValue.user-id          = "_default"
                dynParamValue.prgmName         = "{&program-id}"
                dynParamValue.paramDescription = "User Default"
                .
            FOR EACH ttSubjectParamSet NO-LOCK
                WHERE ttSubjectParamSet.subjectID EQ ttSubject.subjectID,
                EACH dynParamSetDtl NO-LOCK
                WHERE dynParamSetDtl.paramSetID EQ ttSubjectParamSet.paramSetID,
                FIRST dynParam NO-LOCK
                WHERE dynParam.paramID EQ dynParamSetDtl.paramID
                :
                ASSIGN
                    idx                           = idx + 1
                    dynParamValue.paramName[idx]  = dynParamSetDtl.paramName
                    dynParamValue.paramLabel[idx] = dynParamSetDtl.paramLabel
                    dynParamValue.paramValue[idx] = dynParamSetDtl.initialValue
                    .
            END. /* each dynsubjectparamset */
            FIND CURRENT dynParamValue NO-LOCK.
        END. /* not avail */
    END. /* do trans */
    
    cQueryStr = queryStr.
    IF INDEX(queryStr,"[[") NE 0 THEN DO:
        FOR EACH ttSubjectParamSet NO-LOCK
            WHERE ttSubjectParamSet.subjectID EQ ttSubject.subjectID,
            EACH dynParamSetDtl NO-LOCK
            WHERE dynParamSetDtl.paramSetID EQ ttSubjectParamSet.paramSetID,
            FIRST dynParam OF dynParamSetDtl NO-LOCK
            :
            ASSIGN
                cParam = IF dynParamSetDtl.paramName NE "" THEN dynParamSetDtl.paramName
                         ELSE dynParam.paramName
                cParam = "[[" + cParam + "]]"
                .
            IF INDEX(cQueryStr,cParam) NE 0 THEN
            CASE dynParam.dataType:
                WHEN "Character" THEN
                cQueryStr = REPLACE(cQueryStr,cParam,"~"" + dynParamSetDtl.initialValue + "~"").
                WHEN "Date" THEN DO:
                    dtDate = DATE(dynParamSetDtl.initialValue) NO-ERROR.
                    cDate = IF dtDate EQ ? THEN "~?" ELSE STRING(dtDate,dynParam.paramFormat).
                    cQueryStr = REPLACE(cQueryStr,cParam,cDate).
                END. /* date */
                WHEN "DateTime" THEN DO:
                    dtDate = DATE(dynParamSetDtl.initialValue) NO-ERROR.
                    cDate = IF dtDate EQ ? THEN "~?" ELSE STRING(dtDate,dynParam.paramFormat).
                    cQueryStr = REPLACE(cQueryStr,cParam,cDate).
                    cQueryStr = REPLACE(cQueryStr,cParam,dynParamSetDtl.initialValue).
                END. /* date */
                WHEN "Decimal" OR WHEN "Integer" OR WHEN "Logical" THEN
                cQueryStr = REPLACE(cQueryStr,cParam,dynParamSetDtl.initialValue).
            END CASE.
        END. /* each ttSubjectParamSet */
    END. /* if [[ (parameter used) */

    CREATE QUERY hQuery.
    FOR EACH ttSubjectTable
        WHERE ttSubjectTable.subjectID EQ ttSubject.subjectID
           BY ttSubjectTable.sortOrder
        :
        idx = idx + 1.
        CREATE BUFFER hBuffer[idx] FOR TABLE ttSubjectTable.tableName.
        hQuery:ADD-BUFFER(hBuffer[idx]).
    END. /* each ttSubjectTable */
    lOK = hQuery:QUERY-PREPARE(cQueryStr) NO-ERROR.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveParamValues C-Win 
PROCEDURE pSaveParamValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
    
    OUTPUT TO c:\tmp\paramValue.txt.
    PUT UNFORMATTED
        "ID: " ttSubject.subjectID " - "
        "Name: " ttSubject.subjectName
        SKIP(1).
    ASSIGN
        hWidget = FRAME paramFrame:HANDLE
        hWidget = hWidget:FIRST-CHILD
        hWidget = hWidget:FIRST-CHILD
        .
    DO WHILE VALID-HANDLE(hWidget):
        IF hWidget:TYPE NE "BUTTON" AND
           hWidget:TYPE NE "RECTANGLE" AND
           hWidget:TYPE NE "TEXT" THEN DO:
            PUT UNFORMATTED
                hWidget:NAME AT 1
                hWidget:DATA-TYPE AT 20
                hWidget:SCREEN-VALUE AT 40
                hWidget:PRIVATE-DATA AT 60
                .
            FIND FIRST ttAction
                 WHERE ttAction.paramWidget EQ hWidget:HANDLE
                 NO-ERROR.
            IF AVAILABLE ttAction THEN
            PUT UNFORMATTED
                ttAction.paramID AT 80
                ttAction.actionParamID AT 100
                ttAction.initializeProc AT 120
                ttAction.validateProc AT 140
                ttAction.action AT 160
                .
        END.
        hWidget = hWidget:NEXT-SIBLING.
    END.
    OUTPUT CLOSE.
    OS-COMMAND NO-WAIT notepad.exe c:\tmp\paramValue.txt.

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
        FIND FIRST ttSubjectColumn
             WHERE ttSubjectColumn.subjectID   EQ ttSubject.subjectID
               AND ttSubjectColumn.tableDB   EQ cDBName
               AND ttSubjectColumn.tableName EQ cTableName
               AND ttSubjectColumn.fieldName EQ cFieldName
             NO-ERROR.
        IF NOT AVAILABLE ttSubjectColumn THEN NEXT.
        IF ttSubjectColumn.sortOrder  NE idx  OR
          (ttSubjectColumn.columnSize NE 0    AND
           ttSubjectColumn.columnSize NE dSize) THEN DO:
            fSetSaveButton (YES).
            ASSIGN
                ttSubjectColumn.sortOrder  = idx
                ttSubjectColumn.columnSize = dSize
                .
        END. /* if changes */
    END. /* do idx */
    IF lSave THEN
    {&OPEN-QUERY-subjectColumnBrowse}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveSubject C-Win 
PROCEDURE pSaveSubject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bttSubject         FOR ttSubject.
    DEFINE BUFFER bttSubjectTable    FOR ttSubjectTable.
    DEFINE BUFFER bttSubjectWhere    FOR ttSubjectWhere.
    DEFINE BUFFER bttSubjectSort     FOR ttSubjectSort.
    DEFINE BUFFER bttSubjectColumn   FOR ttSubjectColumn.
    DEFINE BUFFER bttSubjectParamSet FOR ttSubjectParamSet.
    
    OS-CREATE-DIR VALUE("users/" + USERID("ASI")).
    OS-CREATE-DIR VALUE("users/" + USERID("ASI") + "/Subject").
    OUTPUT TO VALUE("users/" + USERID("ASI") + "/Subject/ttSubject.d").
    FOR EACH bttSubject:
        EXPORT bttSubject.
    END.
    OUTPUT CLOSE.
    OUTPUT TO VALUE("users/" + USERID("ASI") + "/Subject/ttSubjectTable.d").
    FOR EACH bttSubjectTable:
        EXPORT bttSubjectTable.
    END.
    OUTPUT CLOSE.
    OUTPUT TO VALUE("users/" + USERID("ASI") + "/Subject/ttSubjectWhere.d").
    FOR EACH bttSubjectWhere
        BY bttSubjectWhere.subjectID
        BY bttSubjectWhere.sortOrder:
        EXPORT bttSubjectWhere.
    END.
    OUTPUT CLOSE.
    OUTPUT TO VALUE("users/" + USERID("ASI") + "/Subject/ttSubjectSort.d").
    FOR EACH bttSubjectSort:
        EXPORT bttSubjectSort.
    END.
    OUTPUT CLOSE.
    OUTPUT TO VALUE("users/" + USERID("ASI") + "/Subject/ttSubjectColumn.d").
    FOR EACH bttSubjectColumn:
        EXPORT bttSubjectColumn.
    END.
    OUTPUT CLOSE.
    OUTPUT TO VALUE("users/" + USERID("ASI") + "/Subject/ttSubjectParamSet.d").
    FOR EACH bttSubjectParamSet:
        EXPORT bttSubjectParamSet.
    END.
    OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetGroupListItems C-Win 
PROCEDURE pSetGroupListItems :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cGroups AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER ttSubjectColumn FOR ttSubjectColumn.
    
    cGroups = fJasperGroups().
    /* check for invalid groups */
    FOR EACH ttGroupCalc
        WHERE ttGroupCalc.subjectID EQ ttSubject.subjectID
          AND LOOKUP(ttGroupCalc.groupName,cGroups) EQ 0
        :
        DELETE ttGroupCalc.
    END. /* each grttGroupCalcoupCalc */
    FOR EACH ttSubjectColumn
        :
        ttSubjectColumn.groupCalc = fJasperGroupCalc(ttSubjectColumn.fieldName).
    END. /* each bttSubjectColumn*/
    BROWSE subjectColumnBrowse:REFRESH() NO-ERROR.

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
    &Scoped-define COL 82
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
            
            /* subject table browse */
            RECT-QUERYTABLE:COL    = {&COL}
            RECT-QUERYTABLE:ROW    = {&ROW}
            findType:COL           = RECT-QUERYTABLE:COL + 2
            findType:ROW           = RECT-QUERYTABLE:ROW + .24
            cUseIndexLabel:COL     = RECT-QUERYTABLE:COL + RECT-QUERYTABLE:WIDTH + 1
            cUseIndexLabel:ROW     = RECT-QUERYTABLE:ROW + .24
            cUseIndex:COL          = cUseIndexLabel:COL + cUseIndexLabel:WIDTH
            cUseIndex:ROW          = cUseIndexLabel:ROW
            btnAddUseIndex:COL     = cUseIndex:COL + cUseIndex:WIDTH
            btnAddUseIndex:ROW     = cUseIndex:ROW
            btnRemoveUseIndex:COL  = btnAddUseIndex:COL + btnAddUseIndex:WIDTH
            btnRemoveUseIndex:ROW  = btnAddUseIndex:ROW
            subjectTableBrowse:COL = RECT-QUERYTABLE:COL
            subjectTableBrowse:ROW = RECT-QUERYTABLE:ROW + 1.67
            
            /* subject sort browse */
            RECT-QUERYSORT:COL    = {&COL}
            RECT-QUERYSORT:ROW    = {&ROW}
            sortType:COL          = RECT-QUERYSORT:COL + 9
            sortType:ROW          = RECT-QUERYSORT:ROW + .24
            subjectSortBrowse:COL = RECT-QUERYSORT:COL
            subjectSortBrowse:ROW = RECT-QUERYSORT:ROW + 1.67            

            /* subject column browse */
            RECT-COLUMN:COL         = {&COL}
            RECT-COLUMN:ROW         = {&ROW}
            columnSearch:COL        = RECT-COLUMN:COL + 1
            columnSearch:ROW        = RECT-COLUMN:ROW + .24
            columnMatches:COL       = RECT-COLUMN:COL + 66
            columnMatches:ROW       = RECT-COLUMN:ROW + .24
            subjectColumnBrowse:COL = RECT-COLUMN:COL
            subjectColumnBrowse:ROW = RECT-COLUMN:ROW + 1.67
            
            /* parameter set browse */
            RECT-PARAM:COL      = 40
            RECT-PARAM:ROW      = {&ROW}
            paramSetSearch:COL  = RECT-FIELD:COL + 1
            paramSetSearch:ROW  = RECT-FIELD:ROW + .24
            paramSetMatches:COL = RECT-FIELD:COL + 24
            paramSetMatches:ROW = RECT-FIELD:ROW + .24
            paramSetBrowse:COL  = RECT-FIELD:COL
            paramSetBrowse:ROW  = RECT-FIELD:ROW + 1.67
            
            /* parameter set browse */
            subjectParamSetBrowse:COL = {&COL} - 5
            subjectParamSetBrowse:ROW = {&ROW} + 1.67            
            
            /* save button */
            RECT-SAVE:COL = {&COL}
            RECT-SAVE:ROW = 1
            btnSave:ROW   = RECT-SAVE:ROW + .24
            btnSave:COL   = RECT-SAVE:COL + 1
            
            /* results frame */
            FRAME resultsFrame:COL = 1
            FRAME resultsFrame:ROW = 1
            FRAME resultsFrame:HIDDEN = YES
            btnCloseResults:ROW = 1
            btnSaveResults:ROW  = 1

            /* param frame */
            FRAME paramFrame:COL = 1
            FRAME paramFrame:ROW = 1
            FRAME paramFrame:HIDDEN = YES
            btnCloseParam:ROW = 1
            btnRunResults:ROW  = 1
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
    
    DEFINE BUFFER bttSubjectColumn FOR ttSubjectColumn.
    DEFINE BUFFER bttSubjectSort   FOR ttSubjectSort.
    DEFINE BUFFER bttSubjectTable  FOR ttSubjectTable.
    DEFINE BUFFER bttSubjectWhere  FOR ttSubjectWhere.
    
    CASE subjectSection:
        WHEN "Columns" THEN DO:
            {AOA/includes/pSetOrder.i "bttSubjectColumn" "subjectColumnBrowse"}
        END. /* columns */
        WHEN "Sort" THEN DO:
            {AOA/includes/pSetOrder.i "bttSubjectSort" "subjectSortBrowse"}
        END. /* sort */
        WHEN "Table" THEN DO:
            {AOA/includes/pSetOrder.i "bttSubjectTable" "subjectTableBrowse"}
        END. /* table */
        WHEN "Where" OR WHEN "Parameters" THEN DO:
            {AOA/includes/pSetOrder.i "bttSubjectWhere" "subjectWhereBrowse"}
        END. /* where */
    END CASE.
    fSetSaveButton (YES).
    fShowQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidate C-Win 
PROCEDURE pValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    RUN pParamAction (iphWidget).

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
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT  = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH   = {&WINDOW-NAME}:WIDTH
            FRAME {&FRAME-NAME}:HEIGHT          = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:WIDTH           = {&WINDOW-NAME}:WIDTH
            RECT-QUERYSTR:WIDTH                 = FRAME {&FRAME-NAME}:WIDTH
                                                - RECT-QUERYSTR:COL
            RECT-QUERYSTR:ROW                   = FRAME {&FRAME-NAME}:HEIGHT
                                                - RECT-QUERYSTR:HEIGHT + 1
            btnSyntax:ROW                       = RECT-QUERYSTR:ROW + .72
            queryText:ROW                       = RECT-QUERYSTR:ROW - .23
            queryStr:ROW                        = RECT-QUERYSTR:ROW + .72
            queryStr:WIDTH                      = RECT-QUERYSTR:WIDTH - 7
            cParameter:ROW                      = RECT-QUERYSTR:ROW - 2.38
            cParameterLabel:ROW                 = cParameter:ROW
            btnAddParameter:ROW                 = cParameter:ROW            
            cConstant:ROW                       = RECT-QUERYSTR:ROW - 1.19
            cConstantLabel:ROW                  = cConstant:ROW
            btnAddConstant:ROW                  = cConstant:ROW
            BROWSE subjectBrowse:HEIGHT         = FRAME {&FRAME-NAME}:HEIGHT
                                                - BROWSE subjectBrowse:ROW + 1
            BROWSE subjectBrowse:WIDTH          = IF subjectSection NE "Subject" THEN 37
                                                  ELSE FRAME {&FRAME-NAME}:WIDTH
                                                - BROWSE subjectBrowse:COL
            BROWSE tableBrowse:HEIGHT           = BROWSE subjectBrowse:HEIGHT
            BROWSE fieldBrowse:HEIGHT           = BROWSE subjectBrowse:HEIGHT
            BROWSE subjectTableBrowse:HEIGHT    = RECT-QUERYSTR:ROW
                                                - BROWSE tableBrowse:ROW - .24
            BROWSE subjectWhereBrowse:HEIGHT    = cParameter:ROW
                                                - BROWSE subjectWhereBrowse:ROW - .20
            BROWSE subjectSortBrowse:HEIGHT     = BROWSE subjectTableBrowse:HEIGHT
            BROWSE subjectColumnBrowse:HEIGHT   = BROWSE subjectBrowse:HEIGHT
            BROWSE subjectColumnBrowse:WIDTH    = FRAME {&FRAME-NAME}:WIDTH
                                                - BROWSE subjectColumnBrowse:COL + 1
            BROWSE paramSetBrowse:HEIGHT        = BROWSE subjectBrowse:HEIGHT
            BROWSE subjectParamSetBrowse:HEIGHT = BROWSE subjectBrowse:HEIGHT
            FRAME resultsFrame:HIDDEN           = YES
            FRAME resultsFrame:VIRTUAL-HEIGHT   = FRAME {&FRAME-NAME}:HEIGHT
            FRAME resultsFrame:VIRTUAL-WIDTH    = FRAME {&FRAME-NAME}:WIDTH
            FRAME resultsFrame:HEIGHT           = FRAME {&FRAME-NAME}:HEIGHT
            FRAME resultsFrame:WIDTH            = FRAME {&FRAME-NAME}:WIDTH
            FRAME paramFrame:HIDDEN             = YES
            FRAME paramFrame:VIRTUAL-HEIGHT     = FRAME {&FRAME-NAME}:HEIGHT
            FRAME paramFrame:VIRTUAL-WIDTH      = FRAME {&FRAME-NAME}:WIDTH
            FRAME paramFrame:HEIGHT             = FRAME {&FRAME-NAME}:HEIGHT
            FRAME paramFrame:WIDTH              = FRAME {&FRAME-NAME}:WIDTH
            .
        VIEW FRAME {&FRAME-NAME}.
        IF NOT CAN-DO("Subject,Columns",subjectSection) THEN
        ASSIGN
            RECT-QUERYSTR:HIDDEN = NO
            btnSyntax:HIDDEN     = NO
            queryStr:HIDDEN      = NO
            .
        btnSyntax:MOVE-TO-TOP().
    END. /* do with */
    DO WITH FRAME resultsFrame:
        ASSIGN
            btnCloseResults:COL = FRAME resultsFrame:WIDTH
                                - btnCloseResults:WIDTH
            btnSaveResults:COL  = btnCloseResults:COL
                                - btnSaveResults:WIDTH
            .
        IF VALID-HANDLE(hQueryBrowse) THEN DO:
            ASSIGN
                hQueryBrowse:HEIGHT       = FRAME resultsFrame:HEIGHT - .1
                hQueryBrowse:WIDTH        = FRAME resultsFrame:WIDTH - .32
                FRAME resultsFrame:HIDDEN = NO
                .
        END. /* if valid-handle */
    END. /* do with */
    DO WITH FRAME paramFrame:
        ASSIGN
            btnCloseParam:COL = FRAME paramFrame:WIDTH
                              - btnCloseParam:WIDTH
            btnRunResults:COL = btnCloseParam:COL
                              - btnRunResults:WIDTH
            .
    END. /* do with */
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCreateLabel C-Win 
FUNCTION fCreateLabel RETURNS HANDLE
  (ipcPool AS CHARACTER, iphFrame AS HANDLE, ipcLabel AS CHARACTER, ipdRow AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hLabel AS HANDLE NO-UNDO.
    
    ipcLabel = TRIM(ipcLabel) + ":".
    CREATE TEXT hLabel IN WIDGET-POOL ipcPool
      ASSIGN
        FRAME = iphFrame
        AUTO-RESIZE = YES
        HEIGHT = 1
        WIDTH = FONT-TABLE:GET-TEXT-WIDTH-CHARS(ipcLabel,iphFrame:FONT) + .5
        ROW = ipdRow
        FORMAT = "x(" + STRING(LENGTH(ipcLabel)) + ")"
        SCREEN-VALUE = ipcLabel
        SENSITIVE = YES
        .
  RETURN hLabel.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetUseIndex C-Win 
FUNCTION fGetUseIndex RETURNS CHARACTER
  (ipcTableDB AS CHARACTER, ipcTableName AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cIndexList AS CHARACTER NO-UNDO.
    
    CREATE ALIAS "dictdb" FOR DATABASE VALUE(ipcTableDB).
    RUN nosweat/indxlist.p (ipcTableName, OUTPUT cIndexList).
    
    RETURN cIndexList.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fJasperFields C-Win 
FUNCTION fJasperFields RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFields AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bttSubjectColumn FOR ttSubjectColumn.
        
    FOR EACH bttSubjectColumn
        WHERE bttSubjectColumn.subjectID EQ ttSubject.subjectID
        :
        cFields = cFields + "$F~{" + bttSubjectColumn.fieldName + "},".
    END. /* each ttSubjectColumn*/
    RETURN TRIM(cFields,",").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fJasperGroupCalc C-Win 
FUNCTION fJasperGroupCalc RETURNS CHARACTER
  (ipcField AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cGroupCalc AS CHARACTER NO-UNDO.
    
    FOR EACH ttGroupCalc
        WHERE ttGroupCalc.subjectID EQ ttSubject.subjectID
          AND ttGroupCalc.fieldName EQ ipcField
        :
        IF ttGroupCalc.groupName NE "" THEN 
        cGroupCalc = cGroupCalc
                   + ttGroupCalc.groupName + ","
                   + ttGroupCalc.calcType + ","
                   .
    END. /* each ttgroupcalc */
    RETURN TRIM(cGroupCalc,",").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fJasperGroups C-Win 
FUNCTION fJasperGroups RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cGroups AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER ttSubjectColumn FOR ttSubjectColumn.
    
    /* create list of groups */
    FOR EACH ttSubjectColumn
        WHERE ttSubjectColumn.subjectID EQ ttSubject.subjectID
          AND ttSubjectColumn.isGroup   EQ YES
        :
        cGroups = cGroups + "[Group] " + ttSubjectColumn.fieldLabel + ",".
    END. /* each bttSubjectColumn*/
    RETURN "Column," + cGroups + "Page,Report".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fJasperVariables C-Win 
FUNCTION fJasperVariables RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cVariables  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResetGroup AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cName       AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bttSubjectColumn FOR ttSubjectColumn.
        
    FOR EACH bttSubjectColumn
        WHERE bttSubjectColumn.subjectID EQ ttSubject.subjectID
          AND bttSubjectColumn.groupCalc NE "",
        EACH ttGroupCalc
        WHERE ttGroupCalc.subjectID EQ bttSubjectColumn.subjectID
          AND ttGroupCalc.fieldName EQ bttSubjectColumn.fieldName
        :
        ASSIGN
            cResetGroup = REPLACE(REPLACE(ttGroupCalc.groupName,"[Group] ","")," ","_") + "_Group"
            cName       = ttGroupCalc.fieldName + "_"
                        + IF ttGroupCalc.groupName BEGINS "[Group] " THEN cResetGroup
                          ELSE ttGroupCalc.groupName + "Footer" 
            cVariables  = cVariables + "$V~{" + cName + "},".
                        .
    END. /* each bttSubjectColumn */
    RETURN TRIM (cVariables,",").

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
    
    DEFINE BUFFER ttSubjectTable FOR ttSubjectTable.
    DEFINE BUFFER ttSubjectWhere FOR ttSubjectWhere.
    DEFINE BUFFER ttSubjectSort  FOR ttSubjectSort.
    
    cQueryStr = "FOR".
    FOR EACH ttSubjectTable
        WHERE ttSubjectTable.subjectID EQ ttSubject.subjectID
           BY ttSubjectTable.sortOrder
        :
        cQueryStr = cQueryStr + " "
                  + ttSubjectTable.tableFind + " "
                  + ttSubjectTable.tableName + " "
                  .
        FOR EACH ttSubjectWhere
            WHERE ttSubjectWhere.subjectID    EQ ttSubjectTable.subjectID
              AND ttSubjectWhere.whereTable EQ ttSubjectTable.tableName
               BY ttSubjectWhere.sortOrder
            :
            cQueryStr = cQueryStr + ttSubjectWhere.whereElement + " ".
        END. /* each ttSubjectWhere */
        IF ttSubjectTable.useIndex NE "" THEN
        cQueryStr = cQueryStr + "USE-INDEX " + ttSubjectTable.useIndex.
        cQueryStr = TRIM(cQueryStr) + ", ".
    END. /* each ttSubjectTable */
    cQueryStr = TRIM(cQueryStr,", ").
    FOR EACH ttSubjectSort
        WHERE ttSubjectSort.subjectID EQ ttSubject.subjectID
           BY ttSubjectSort.sortOrder
        :
        cQueryStr = cQueryStr + " BY " + ttSubjectSort.sortBy.
    END. /* each ttSubjectSort */
    
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
        ttSubject.queryStr = queryStr
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
    RETURN TRIM(ttSubjectSort.tableName + "."
              + TRIM(ttSubjectSort.fieldName + " "
              + STRING(ttSubjectSort.sortAsc,"/DESCENDING")))
              .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

