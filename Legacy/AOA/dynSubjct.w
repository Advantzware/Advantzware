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
&Scoped-define showFields svShowAll svShowReportHeader svShowPageHeader ~
svShowGroupHeader svShowGroupFooter svShowPageFooter svShowReportFooter ~
svShowParameters

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cDataType          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFormat            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLabel             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPoolName          AS CHARACTER NO-UNDO.
DEFINE VARIABLE hAppSrvBin         AS HANDLE    NO-UNDO.
DEFINE VARIABLE hJasper            AS HANDLE    NO-UNDO.
DEFINE VARIABLE hQueryBrowse       AS HANDLE    NO-UNDO.
DEFINE VARIABLE i                  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iOrder             AS INTEGER   NO-UNDO.
DEFINE VARIABLE iUserSecurityLevel AS INTEGER   NO-UNDO.
DEFINE VARIABLE lContinue          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lSave              AS LOGICAL   NO-UNDO.
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
DEFINE TEMP-TABLE ttSubjectColumn   NO-UNDO LIKE dynSubjectColumn.
DEFINE TEMP-TABLE ttSubjectParamSet NO-UNDO LIKE dynSubjectParamSet.
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
{AOA/tempTable/ttAction.i}

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

{AOA/includes/dynFuncs.i}

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
ttSubjectColumn ttSubjectParamSet ttSubjectTable ttSubjectWhere ttTable

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
&Scoped-define FIELDS-IN-QUERY-subjectColumnBrowse ttSubjectColumn.fieldName ttSubjectColumn.fieldLabel ttSubjectColumn.sortCol ttSubjectColumn.isGroup ttSubjectColumn.groupLabel ttSubjectColumn.fieldFormat ttSubjectColumn.groupCalc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-subjectColumnBrowse ttSubjectColumn.fieldLabel ttSubjectColumn.sortCol ttSubjectColumn.isGroup ttSubjectColumn.groupLabel ttSubjectColumn.fieldFormat   
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
    ~{&OPEN-QUERY-subjectTableBrowse}~
    ~{&OPEN-QUERY-subjectWhereBrowse}~
    ~{&OPEN-QUERY-tableBrowse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnResults btnTest subjectSection ~
btnOuterJoin btnNow btnToday btnTime btnDateTime subjectSearch ~
subjectMatches tableSearch tableMatches tableList btnOF tableListOf ~
btnWhere btnMatches subjectBrowse tableBrowse subjectWhereBrowse btnBegins ~
btnAND btnOR btnEQ btnNE btnLT btnGT fieldSearch btnSyntax fieldMatches ~
btnLE btnGE paramSetBrowse fieldBrowse btnPlus btnMinus btnMultiply ~
btnDivide btnYes btnNo btnDate cUseIndex columnSearch columnMatches btnDec ~
subjectColumnBrowse findType btnInt subjectTableBrowse btnStr btnSubstr ~
cParameter btnOpen btnClose cConstant btnPeriod btnDouble btnComma ~
btnSingle btnAddSelections paramSetSearch paramSetMatches queryStr ~
subjectParamSetBrowse btnGroupCalc btnAddUseIndex btnRemoveUseIndex ~
btnAddParameter btnSave btnRemoveSelection btnMoveDown btnAddConstant ~
btnMoveUp btnRemove btnAdd btnCopy btnDelete btnUpdate cParameterLabel ~
cConstantLabel 
&Scoped-Define DISPLAYED-OBJECTS subjectSection subjectSearch ~
subjectMatches tableSearch tableMatches tableList tableListOf fieldSearch ~
fieldMatches cUseIndex columnSearch columnMatches findType cParameter ~
cConstant paramSetSearch paramSetMatches queryStr cUseIndexLabel ~
cParameterLabel cConstantLabel queryText 

/* Custom List Definitions                                              */
/* allSection,tableSection,whereSection,parameterSection,columnsSection,subjectSection */
&Scoped-define allSection RECT-TABLE RECT-FIELD RECT-QUERYTABLE ~
RECT-QUERYSTR RECT-COLUMN RECT-PANEL RECT-SAVE RECT-PARAM RECT-PLAY ~
btnOuterJoin btnNow btnToday btnTime btnDateTime tableSearch tableMatches ~
tableList btnOF tableListOf btnWhere btnMatches tableBrowse ~
subjectWhereBrowse btnBegins btnAND btnOR btnEQ btnNE btnLT btnGT ~
fieldSearch btnSyntax fieldMatches btnLE btnGE paramSetBrowse fieldBrowse ~
btnPlus btnMinus btnMultiply btnDivide btnYes btnNo btnDate cUseIndex ~
columnSearch columnMatches btnDec subjectColumnBrowse findType btnInt ~
subjectTableBrowse btnStr btnSubstr cParameter btnOpen btnClose cConstant ~
btnPeriod btnDouble btnComma btnSingle btnAddSelections paramSetSearch ~
paramSetMatches queryStr subjectParamSetBrowse btnCancel btnGroupCalc ~
btnAddUseIndex btnRemoveUseIndex btnAddParameter btnSave btnRemoveSelection ~
btnMoveDown btnAddConstant btnMoveUp btnRemove btnAdd btnCopy btnDelete ~
btnReset btnUpdate cUseIndexLabel cParameterLabel cConstantLabel queryText 
&Scoped-define tableSection RECT-TABLE RECT-QUERYTABLE RECT-QUERYSTR ~
RECT-PLAY tableSearch tableMatches tableBrowse btnSyntax cUseIndex findType ~
subjectTableBrowse btnAddSelections queryStr btnAddUseIndex ~
btnRemoveUseIndex btnRemoveSelection btnMoveDown btnMoveUp btnRemove ~
cUseIndexLabel queryText 
&Scoped-define whereSection RECT-FIELD RECT-QUERYSTR RECT-PARAM RECT-PLAY ~
btnOuterJoin btnNow btnToday btnTime btnDateTime tableList btnOF ~
tableListOf btnWhere btnMatches subjectWhereBrowse btnBegins btnAND btnOR ~
btnEQ btnNE btnLT btnGT fieldSearch btnSyntax fieldMatches btnLE btnGE ~
fieldBrowse btnPlus btnMinus btnMultiply btnDivide btnYes btnNo btnDate ~
btnDec btnInt btnStr btnSubstr cParameter btnOpen btnClose cConstant ~
btnPeriod btnDouble btnComma btnSingle btnAddSelections paramSetMatches ~
queryStr btnAddParameter btnRemoveSelection btnMoveDown btnAddConstant ~
btnMoveUp btnRemove cParameterLabel cConstantLabel queryText 
&Scoped-define parameterSection RECT-QUERYSTR RECT-PARAM RECT-PLAY ~
paramSetBrowse paramSetSearch paramSetMatches queryStr ~
subjectParamSetBrowse queryText 
&Scoped-define columnsSection RECT-FIELD RECT-COLUMN RECT-PARAM RECT-PLAY ~
fieldSearch fieldMatches fieldBrowse columnSearch columnMatches ~
subjectColumnBrowse btnAddSelections paramSetMatches btnGroupCalc ~
btnRemoveSelection btnMoveDown btnMoveUp btnRemove 
&Scoped-define subjectSection RECT-PANEL btnCancel btnAdd btnCopy btnDelete ~
btnReset btnUpdate 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fSetShowAll C-Win 
FUNCTION fSetShowAll RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fShowQuery C-Win 
FUNCTION fShowQuery RETURNS LOGICAL
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
     SIZE 8 BY 1.05 TOOLTIP "NOW".

DEFINE BUTTON btnOF 
     LABEL "OF" 
     SIZE 5 BY 1.05 TOOLTIP "OF".

DEFINE BUTTON btnOpen 
     LABEL "(" 
     SIZE 5 BY 1.05 TOOLTIP "Open Parentheses".

DEFINE BUTTON btnOR 
     LABEL "OR" 
     SIZE 5 BY 1.05 TOOLTIP "OR".

DEFINE BUTTON btnOuterJoin 
     LABEL "OUTER-JOIN" 
     SIZE 14 BY 1.05 TOOLTIP "OUTER-JOIN".

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
     IMAGE-UP FILE "Graphics/32x32/media_play.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Results" 
     SIZE 8 BY 1.91 TOOLTIP "Subject Results".

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

DEFINE BUTTON btnTest 
     LABEL "Test" 
     SIZE 7 BY 1 TOOLTIP "Test".

DEFINE BUTTON btnTime 
     LABEL "TIME" 
     SIZE 8 BY 1.05 TOOLTIP "TIME".

DEFINE BUTTON btnToday 
     LABEL "TODAY" 
     SIZE 10 BY 1.05 TOOLTIP "TODAY".

DEFINE BUTTON btnUpdate 
     IMAGE-UP FILE "Graphics/32x32/pencil.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/pencil_disabled.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Update" 
     SIZE 8 BY 1.91 TOOLTIP "Update/Save".

DEFINE BUTTON btnWhere 
     LABEL "WHERE" 
     SIZE 10 BY 1.05 TOOLTIP "WHERE".

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

DEFINE VARIABLE subjectSection AS CHARACTER INITIAL "Subject" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Sub&ject", "Subject",
"&Table", "Table",
"&Where", "Where",
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

DEFINE RECTANGLE RECT-PANEL
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 58 BY 2.38
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-PARAM
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 36 BY 1.43.

DEFINE RECTANGLE RECT-PLAY
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 9.8 BY 2.38.

DEFINE RECTANGLE RECT-QUERY
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 37 BY 1.43.

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

DEFINE VARIABLE svShowAll AS LOGICAL INITIAL yes 
     LABEL "Show ALL" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .81 NO-UNDO.

DEFINE VARIABLE svShowGroupFooter AS LOGICAL INITIAL yes 
     LABEL "Group Footer" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE svShowGroupHeader AS LOGICAL INITIAL yes 
     LABEL "Group Header" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE svShowPageFooter AS LOGICAL INITIAL yes 
     LABEL "Page Footer" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE svShowPageHeader AS LOGICAL INITIAL yes 
     LABEL "Page Header" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE svShowParameters AS LOGICAL INITIAL yes 
     LABEL "Parameters" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81 NO-UNDO.

DEFINE VARIABLE svShowReportFooter AS LOGICAL INITIAL yes 
     LABEL "Report Footer" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE svShowReportHeader AS LOGICAL INITIAL yes 
     LABEL "Report Header" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE BUTTON btnCloseParam 
     IMAGE-UP FILE "AOA/images/navigate_cross.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Close Parameter Screen" 
     SIZE 4.4 BY .95 TOOLTIP "Close Parameter Screen".

DEFINE BUTTON btnCSV 
     IMAGE-UP FILE "Graphics/32x32/csv.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "csv" 
     SIZE 8 BY 1.91 TOOLTIP "Excel CSV".

DEFINE BUTTON btnDOCX 
     IMAGE-UP FILE "Graphics/32x32/docx.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Word DOCX".

DEFINE BUTTON btnHTML 
     IMAGE-UP FILE "Graphics/32x32/html_tag.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "HTML".

DEFINE BUTTON btnPDF 
     IMAGE-UP FILE "Graphics/32x32/pdf.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "PDF".

DEFINE BUTTON btnRunResults 
     IMAGE-UP FILE "Graphics/32x32/table.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Run Results" 
     SIZE 8 BY 1.91 TOOLTIP "Run Results".

DEFINE BUTTON btnView 
     IMAGE-UP FILE "Graphics/32x32/jss_icon_32.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Jasper Viewer".

DEFINE BUTTON btnXLS 
     IMAGE-UP FILE "Graphics/32x32/xls.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Excel XLS".

DEFINE RECTANGLE RECT-PANEL-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 58 BY 2.38.

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
ttSubjectColumn.sortCol
ttSubjectColumn.isGroup VIEW-AS TOGGLE-BOX
ttSubjectColumn.groupLabel
ttSubjectColumn.fieldFormat
ttSubjectColumn.groupCalc
ENABLE
ttSubjectColumn.fieldLabel
ttSubjectColumn.sortCol
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
     btnResults AT ROW 1.24 COL 83 HELP
          "Jasper Viewer" WIDGET-ID 250
     btnTest AT ROW 1.24 COL 3 WIDGET-ID 278
     subjectSection AT ROW 2.19 COL 3 HELP
          "Select Section" NO-LABEL WIDGET-ID 30
     btnOuterJoin AT ROW 2.91 COL 106 WIDGET-ID 274
     btnNow AT ROW 2.91 COL 120 WIDGET-ID 194
     btnToday AT ROW 2.91 COL 128 WIDGET-ID 190
     btnTime AT ROW 2.91 COL 138 WIDGET-ID 192
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
     btnEQ AT ROW 7.67 COL 150 WIDGET-ID 68
     btnNE AT ROW 7.67 COL 155 WIDGET-ID 70
     btnLT AT ROW 8.86 COL 150 WIDGET-ID 72
     btnGT AT ROW 8.86 COL 155 WIDGET-ID 74
     fieldSearch AT ROW 9.81 COL 39 COLON-ALIGNED HELP
          "Enter Field Search" NO-LABEL WIDGET-ID 50
     btnSyntax AT ROW 23.86 COL 78 WIDGET-ID 202
     fieldMatches AT ROW 9.81 COL 64 HELP
          "Select for Table Search Matches" WIDGET-ID 52
     btnLE AT ROW 10.05 COL 150 WIDGET-ID 76
     btnGE AT ROW 10.05 COL 155 WIDGET-ID 78
     paramSetBrowse AT ROW 11 COL 2 WIDGET-ID 1000
     fieldBrowse AT ROW 11 COL 40 WIDGET-ID 700
     btnPlus AT ROW 11.24 COL 150 WIDGET-ID 158
     btnMinus AT ROW 11.24 COL 155 WIDGET-ID 156
     btnMultiply AT ROW 12.43 COL 150 WIDGET-ID 160
     btnDivide AT ROW 12.43 COL 155 WIDGET-ID 154
     btnYes AT ROW 13.62 COL 150 WIDGET-ID 172
     btnNo AT ROW 13.62 COL 155 WIDGET-ID 174
     btnDate AT ROW 14.81 COL 150 WIDGET-ID 166
     cUseIndex AT ROW 15.52 COL 12 COLON-ALIGNED NO-LABEL WIDGET-ID 262
     columnSearch AT ROW 15.52 COL 70 COLON-ALIGNED HELP
          "Enter Column Search" NO-LABEL WIDGET-ID 112
     columnMatches AT ROW 15.52 COL 137 HELP
          "Select for Column Search Matches" WIDGET-ID 110
     btnDec AT ROW 16 COL 150 WIDGET-ID 164
     subjectColumnBrowse AT ROW 16.71 COL 71 WIDGET-ID 900
     findType AT ROW 16.95 COL 4 NO-LABEL WIDGET-ID 26
     btnInt AT ROW 17.19 COL 150 WIDGET-ID 162
     subjectTableBrowse AT ROW 18.38 COL 2 WIDGET-ID 400
     btnStr AT ROW 18.38 COL 150 WIDGET-ID 168
     btnSubstr AT ROW 19.57 COL 150 WIDGET-ID 170
     cParameter AT ROW 20.76 COL 91 COLON-ALIGNED HELP
          "Select Parameter Type" NO-LABEL WIDGET-ID 204
     btnOpen AT ROW 20.76 COL 150 WIDGET-ID 94
     btnClose AT ROW 20.76 COL 155 WIDGET-ID 96
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     cConstant AT ROW 21.95 COL 91 COLON-ALIGNED NO-LABEL WIDGET-ID 176
     btnPeriod AT ROW 21.95 COL 150 WIDGET-ID 236
     btnDouble AT ROW 21.95 COL 152.4 WIDGET-ID 240
     btnComma AT ROW 21.95 COL 155 WIDGET-ID 242
     btnSingle AT ROW 21.95 COL 157.4 WIDGET-ID 244
     btnAddSelections AT ROW 7.43 COL 77 HELP
          "Add Selections" WIDGET-ID 200
     paramSetSearch AT ROW 23.86 COL 3 HELP
          "Enter Field Search" NO-LABEL WIDGET-ID 258
     paramSetMatches AT ROW 23.86 COL 26 HELP
          "Select for Table Search Matches" WIDGET-ID 256
     queryStr AT ROW 23.86 COL 83 NO-LABEL WIDGET-ID 4
     subjectParamSetBrowse AT ROW 25.29 COL 2 WIDGET-ID 1100
     btnCancel AT ROW 1.24 COL 131 HELP
          "Cancel" WIDGET-ID 120
     btnGroupCalc AT ROW 5.52 COL 77 HELP
          "Group Calculations" WIDGET-ID 272
     btnAddUseIndex AT ROW 15.52 COL 38 WIDGET-ID 268
     btnRemoveUseIndex AT ROW 15.52 COL 43 WIDGET-ID 270
     btnAddParameter AT ROW 20.76 COL 145 WIDGET-ID 208
     btnSave AT ROW 1.24 COL 142 HELP
          "Update/Save" WIDGET-ID 248
     btnRemoveSelection AT ROW 13.62 COL 77 HELP
          "Remove Selections" WIDGET-ID 198
     btnMoveDown AT ROW 11.71 COL 77 HELP
          "Move Down" WIDGET-ID 62
     btnAddConstant AT ROW 21.95 COL 145 WIDGET-ID 180
     btnMoveUp AT ROW 9.33 COL 77 HELP
          "Move Up" WIDGET-ID 64
     btnRemove AT ROW 10.52 COL 77 HELP
          "Remove" WIDGET-ID 66
     btnAdd AT ROW 1.24 COL 99 HELP
          "Add" WIDGET-ID 118
     btnCopy AT ROW 1.24 COL 107 HELP
          "Copy" WIDGET-ID 122
     btnDelete AT ROW 1.24 COL 115 HELP
          "Delete" WIDGET-ID 124
     btnReset AT ROW 1.24 COL 123 HELP
          "Reset" WIDGET-ID 126
     btnUpdate AT ROW 1.24 COL 91 HELP
          "Update/Save" WIDGET-ID 128
     cUseIndexLabel AT ROW 15.52 COL 2 NO-LABEL WIDGET-ID 266
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
     RECT-QUERYTABLE AT ROW 16.71 COL 2 WIDGET-ID 54
     RECT-QUERYSTR AT ROW 23.14 COL 77 WIDGET-ID 58
     RECT-COLUMN AT ROW 15.29 COL 71 WIDGET-ID 114
     RECT-PANEL AT ROW 1 COL 82 WIDGET-ID 130
     RECT-SAVE AT ROW 1 COL 141 WIDGET-ID 246
     RECT-PARAM AT ROW 23.62 COL 2 WIDGET-ID 260
     RECT-PLAY AT ROW 1 COL 82 WIDGET-ID 276
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.

DEFINE FRAME resultsFrame
     btnCloseResults AT ROW 1 COL 6 HELP
          "Jasper Viewer" WIDGET-ID 252
     btnSaveResults AT ROW 1 COL 2 HELP
          "Jasper Viewer" WIDGET-ID 254
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 66 ROW 22.67
         SIZE 10 BY 2.38
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 1200.

DEFINE FRAME paramFrame
     btnCloseParam AT ROW 1.24 COL 4 HELP
          "Jasper Viewer" WIDGET-ID 252
     btnRunResults AT ROW 1.48 COL 10.4 HELP
          "Jasper Viewer" WIDGET-ID 266
     btnCSV AT ROW 1.48 COL 18.4 HELP
          "Excel CSV" WIDGET-ID 258
     btnDOCX AT ROW 1.48 COL 34.4 HELP
          "Word DOCX" WIDGET-ID 260
     btnHTML AT ROW 1.48 COL 50.4 HELP
          "HTML" WIDGET-ID 262
     btnPDF AT ROW 1.48 COL 42.4 HELP
          "PDF" WIDGET-ID 264
     btnView AT ROW 1.48 COL 58.4 HELP
          "Jasper Viewer" WIDGET-ID 268
     btnXLS AT ROW 1.48 COL 26.4 HELP
          "Excel XLS" WIDGET-ID 270
     RECT-PANEL-2 AT ROW 1.24 COL 9 WIDGET-ID 256
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 82 ROW 9.81
         SIZE 67 BY 11.91
         FGCOLOR 1  WIDGET-ID 1300.

DEFINE FRAME frameShow
     svShowAll AT ROW 1.24 COL 2 WIDGET-ID 18
     svShowReportHeader AT ROW 2.19 COL 5 WIDGET-ID 2
     svShowPageHeader AT ROW 3.14 COL 5 WIDGET-ID 6
     svShowGroupHeader AT ROW 4.1 COL 5 WIDGET-ID 10
     svShowGroupFooter AT ROW 5.05 COL 5 WIDGET-ID 12
     svShowPageFooter AT ROW 6 COL 5 WIDGET-ID 8
     svShowReportFooter AT ROW 6.95 COL 5 WIDGET-ID 4
     svShowParameters AT ROW 7.91 COL 5 WIDGET-ID 16
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 3.86
         SIZE 23 BY 8.81
         BGCOLOR 15 
         TITLE "Show Sections" WIDGET-ID 1400.


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
ASSIGN FRAME frameShow:FRAME = FRAME paramFrame:HANDLE
       FRAME paramFrame:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME resultsFrame:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB subjectBrowse btnMatches DEFAULT-FRAME */
/* BROWSE-TAB tableBrowse subjectBrowse DEFAULT-FRAME */
/* BROWSE-TAB subjectWhereBrowse tableBrowse DEFAULT-FRAME */
/* BROWSE-TAB paramSetBrowse btnGE DEFAULT-FRAME */
/* BROWSE-TAB fieldBrowse paramSetBrowse DEFAULT-FRAME */
/* BROWSE-TAB subjectColumnBrowse btnDec DEFAULT-FRAME */
/* BROWSE-TAB subjectTableBrowse btnInt DEFAULT-FRAME */
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
   1 2 3 5                                                              */
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
   1 5                                                                  */
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
   1 2 3 5                                                              */
ASSIGN 
       btnMoveDown:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnMoveUp IN FRAME DEFAULT-FRAME
   1 2 3 5                                                              */
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

/* SETTINGS FOR BUTTON btnOuterJoin IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnOuterJoin:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnPeriod IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnPeriod:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnPlus IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnPlus:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnRemove IN FRAME DEFAULT-FRAME
   1 2 3 5                                                              */
ASSIGN 
       btnRemove:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnRemoveSelection IN FRAME DEFAULT-FRAME
   1 2 3 5                                                              */
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
   1 2 3                                                                */
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
   NO-ENABLE ALIGN-L 1 2                                                */
/* SETTINGS FOR BROWSE fieldBrowse IN FRAME DEFAULT-FRAME
   1 3 5                                                                */
ASSIGN 
       fieldBrowse:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       fieldBrowse:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR TOGGLE-BOX fieldMatches IN FRAME DEFAULT-FRAME
   1 3 5                                                                */
ASSIGN 
       fieldMatches:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN fieldSearch IN FRAME DEFAULT-FRAME
   1 3 5                                                                */
ASSIGN 
       fieldSearch:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RADIO-SET findType IN FRAME DEFAULT-FRAME
   1 2                                                                  */
ASSIGN 
       findType:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BROWSE paramSetBrowse IN FRAME DEFAULT-FRAME
   1 4                                                                  */
ASSIGN 
       paramSetBrowse:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

/* SETTINGS FOR TOGGLE-BOX paramSetMatches IN FRAME DEFAULT-FRAME
   1 3 4 5                                                              */
ASSIGN 
       paramSetMatches:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN paramSetSearch IN FRAME DEFAULT-FRAME
   ALIGN-L 1 4                                                          */
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
   NO-ENABLE 1 3 5                                                      */
ASSIGN 
       RECT-FIELD:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-PANEL IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 6                                                        */
/* SETTINGS FOR RECTANGLE RECT-PARAM IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 3 4 5                                                    */
ASSIGN 
       RECT-PARAM:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-PLAY IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 2 3 4 5                                                  */
/* SETTINGS FOR RECTANGLE RECT-QUERY IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
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

/* SETTINGS FOR BROWSE subjectColumnBrowse IN FRAME DEFAULT-FRAME
   1 5                                                                  */
ASSIGN 
       subjectColumnBrowse:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       subjectColumnBrowse:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 1.

/* SETTINGS FOR BROWSE subjectParamSetBrowse IN FRAME DEFAULT-FRAME
   1 4                                                                  */
ASSIGN 
       subjectParamSetBrowse:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

/* SETTINGS FOR FILL-IN subjectSearch IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
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

/* SETTINGS FOR FRAME frameShow
                                                                        */
/* SETTINGS FOR FRAME paramFrame
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME paramFrame:HIDDEN           = TRUE.

ASSIGN 
       btnView:AUTO-RESIZE IN FRAME paramFrame      = TRUE.

/* SETTINGS FOR RECTANGLE RECT-PANEL-2 IN FRAME paramFrame
   NO-ENABLE                                                            */
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
ON CHOOSE OF btnCloseParam IN FRAME paramFrame /* Close Parameter Screen */
DO:
    DELETE WIDGET-POOL cPoolName NO-ERROR.
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
    DELETE WIDGET-POOL cPoolName NO-ERROR.
    ASSIGN
        FRAME resultsFrame:HIDDEN = YES
        FRAME paramFrame:HIDDEN   = YES
        .
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


&Scoped-define FRAME-NAME paramFrame
&Scoped-define SELF-NAME btnCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCSV C-Win
ON CHOOSE OF btnCSV IN FRAME paramFrame /* csv */
DO:
    RUN pRunSubject (YES, "CSV").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete C-Win
ON CHOOSE OF btnDelete IN FRAME DEFAULT-FRAME /* Delete */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME paramFrame
&Scoped-define SELF-NAME btnDOCX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDOCX C-Win
ON CHOOSE OF btnDOCX IN FRAME paramFrame
DO:
    RUN pRunSubject (YES, "DOCX").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnGroupCalc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGroupCalc C-Win
ON CHOOSE OF btnGroupCalc IN FRAME DEFAULT-FRAME
DO:
    IF AVAILABLE ttSubjectColumn THEN
    RUN pJasperGroupCalc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME paramFrame
&Scoped-define SELF-NAME btnHTML
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHTML C-Win
ON CHOOSE OF btnHTML IN FRAME paramFrame
DO:
    RUN pRunSubject (YES, "HTML").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
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


&Scoped-define FRAME-NAME paramFrame
&Scoped-define SELF-NAME btnPDF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPDF C-Win
ON CHOOSE OF btnPDF IN FRAME paramFrame
DO:
    RUN pRunSubject (YES, "PDF").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnRemove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemove C-Win
ON CHOOSE OF btnRemove IN FRAME DEFAULT-FRAME
DO:
    CASE subjectSection:
        WHEN "Columns" THEN
            IF AVAILABLE ttSubjectColumn THEN
            APPLY "DEFAULT-ACTION":U TO subjectColumnBrowse.
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
    RUN pGetDynParamValue.
    RUN AOA/dynRun.w PERSISTENT ("{&program-id}", ROWID(dynParamValue)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME paramFrame
&Scoped-define SELF-NAME btnRunResults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRunResults C-Win
ON CHOOSE OF btnRunResults IN FRAME paramFrame /* Run Results */
DO:
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


&Scoped-define SELF-NAME btnTest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTest C-Win
ON CHOOSE OF btnTest IN FRAME DEFAULT-FRAME /* Test */
DO:
    RUN pCreateDynParameters (FRAME paramFrame:HANDLE).
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


&Scoped-define FRAME-NAME paramFrame
&Scoped-define SELF-NAME btnView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnView C-Win
ON CHOOSE OF btnView IN FRAME paramFrame
DO:
    RUN pRunSubject (YES, "View").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnWhere
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnWhere C-Win
ON CHOOSE OF btnWhere IN FRAME DEFAULT-FRAME /* WHERE */
,btnEQ,btnNE,btnLT,btnGT,btnLE,btnGE,btnOpen,btnClose,btnDate,btnDec,btnInt~
,btnStr,btnSubstr,btnYes,btnNo,btnAND,btnOR,btnBegins,btnMatches,btnPlus~
,btnMinus,btnMultiply,btnDivide,btnNow,btnTime,btnToday,btnDateTime~
,btnPeriod,btnDouble,btnComma,btnSingle,btnOuterJoin
DO:
    RUN pAddWhere (tableList, SELF:LABEL, "").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME paramFrame
&Scoped-define SELF-NAME btnXLS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnXLS C-Win
ON CHOOSE OF btnXLS IN FRAME paramFrame
DO:
    RUN pRunSubject (YES, "XLS").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
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


&Scoped-define BROWSE-NAME subjectBrowse
&Scoped-define SELF-NAME subjectBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL subjectBrowse C-Win
ON VALUE-CHANGED OF subjectBrowse IN FRAME DEFAULT-FRAME /* Subject */
DO:
    {&OPEN-QUERY-subjectTableBrowse}
    IF AVAILABLE ttSubjectTable THEN
    APPLY "VALUE-CHANGED":U TO BROWSE subjectTableBrowse.
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


&Scoped-define FRAME-NAME frameShow
&Scoped-define SELF-NAME svShowAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowAll C-Win
ON VALUE-CHANGED OF svShowAll IN FRAME frameShow /* Show ALL */
DO:
    ASSIGN {&SELF-NAME}
        svShowReportHeader = {&SELF-NAME}
        svShowParameters   = {&SELF-NAME}
        svShowPageHeader   = {&SELF-NAME}
        svShowGroupHeader  = {&SELF-NAME}
        svShowGroupFooter  = {&SELF-NAME}
        svShowPageFooter   = {&SELF-NAME}
        svShowReportFooter = {&SELF-NAME}
        .
    DISPLAY {&showFields} WITH FRAME frameShow.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowGroupFooter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowGroupFooter C-Win
ON VALUE-CHANGED OF svShowGroupFooter IN FRAME frameShow /* Group Footer */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowGroupHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowGroupHeader C-Win
ON VALUE-CHANGED OF svShowGroupHeader IN FRAME frameShow /* Group Header */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowPageFooter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowPageFooter C-Win
ON VALUE-CHANGED OF svShowPageFooter IN FRAME frameShow /* Page Footer */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowPageHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowPageHeader C-Win
ON VALUE-CHANGED OF svShowPageHeader IN FRAME frameShow /* Page Header */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowParameters
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowParameters C-Win
ON VALUE-CHANGED OF svShowParameters IN FRAME frameShow /* Parameters */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowReportFooter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowReportFooter C-Win
ON VALUE-CHANGED OF svShowReportFooter IN FRAME frameShow /* Report Footer */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowReportHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowReportHeader C-Win
ON VALUE-CHANGED OF svShowReportHeader IN FRAME frameShow /* Report Header */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME tableBrowse
&Scoped-define FRAME-NAME DEFAULT-FRAME
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

{AOA/includes/dynProcs.i "tt"}

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
          tableList tableListOf fieldSearch fieldMatches cUseIndex columnSearch 
          columnMatches findType cParameter cConstant paramSetSearch 
          paramSetMatches queryStr cUseIndexLabel cParameterLabel cConstantLabel 
          queryText 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnResults btnTest subjectSection btnOuterJoin btnNow btnToday btnTime 
         btnDateTime subjectSearch subjectMatches tableSearch tableMatches 
         tableList btnOF tableListOf btnWhere btnMatches subjectBrowse 
         tableBrowse subjectWhereBrowse btnBegins btnAND btnOR btnEQ btnNE 
         btnLT btnGT fieldSearch btnSyntax fieldMatches btnLE btnGE 
         paramSetBrowse fieldBrowse btnPlus btnMinus btnMultiply btnDivide 
         btnYes btnNo btnDate cUseIndex columnSearch columnMatches btnDec 
         subjectColumnBrowse findType btnInt subjectTableBrowse btnStr 
         btnSubstr cParameter btnOpen btnClose cConstant btnPeriod btnDouble 
         btnComma btnSingle btnAddSelections paramSetSearch paramSetMatches 
         queryStr subjectParamSetBrowse btnGroupCalc btnAddUseIndex 
         btnRemoveUseIndex btnAddParameter btnSave btnRemoveSelection 
         btnMoveDown btnAddConstant btnMoveUp btnRemove btnAdd btnCopy 
         btnDelete btnUpdate cParameterLabel cConstantLabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY svShowAll svShowReportHeader svShowPageHeader svShowGroupHeader 
          svShowGroupFooter svShowPageFooter svShowReportFooter svShowParameters 
      WITH FRAME frameShow IN WINDOW C-Win.
  ENABLE svShowAll svShowReportHeader svShowPageHeader svShowGroupHeader 
         svShowGroupFooter svShowPageFooter svShowReportFooter svShowParameters 
      WITH FRAME frameShow IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frameShow}
  ENABLE btnCloseParam btnRunResults btnCSV btnDOCX btnHTML btnPDF btnView 
         btnXLS 
      WITH FRAME paramFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-paramFrame}
  ENABLE btnCloseResults btnSaveResults 
      WITH FRAME resultsFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-resultsFrame}
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
    IF CAN-FIND(FIRST ttSubjectColumn
            WHERE ttSubjectColumn.subjectID   EQ ttSubject.subjectID
              AND ttSubjectColumn.tableName EQ ttSubjectTable.tableName) THEN DO:
        FOR EACH ttSubjectColumn
            WHERE ttSubjectColumn.subjectID   EQ ttSubject.subjectID
              AND ttSubjectColumn.tableName EQ ttSubjectTable.tableName
            :
            DELETE ttSubjectColumn.
        END. /* each ttSubjectColumn*/
        subjectSection = "Column".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetDynParamValue C-Win 
PROCEDURE pGetDynParamValue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND FIRST dynParamValue NO-LOCK
         WHERE dynParamValue.subjectID    EQ ttSubject.subjectID
           AND dynParamValue.user-id      EQ "_default"
           AND dynParamValue.prgmName     EQ "{&program-id}"
           AND dynParamValue.paramValueID EQ 0
         NO-ERROR.
    IF NOT AVAILABLE dynParamValue THEN
    RUN pSetDynParamValue.

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
           BY ttSubjectTable.sortOrder
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
        tableList:INNER-LINES    = tableList:NUM-ITEMS
        tableList:SCREEN-VALUE   = tableList:ENTRY(1)
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
        fSetSaveButton (YES).
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
    DEFINE BUFFER bttSubjectTable  FOR ttSubjectTable.
    DEFINE BUFFER bttSubjectWhere  FOR ttSubjectWhere.
    
    CASE subjectSection:
        WHEN "Columns" THEN DO:
            {AOA/includes/pMove.i "ttSubjectColumn" "subjectColumnBrowse"}
        END. /* columns */
        WHEN "Table" THEN DO:
            {AOA/includes/pMove.i "ttSubjectTable" "subjectTableBrowse"}
        END. /* table */
        WHEN "Where" THEN DO:
            {AOA/includes/pMove.i "ttSubjectWhere" "subjectWhereBrowse" tableList}
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
        CASE subjectSection:
            WHEN "Columns" THEN DO:
                DO idx = 1 TO subjectColumnBrowse:NUM-SELECTED-ROWS:
                    subjectColumnBrowse:FETCH-SELECTED-ROW(idx).
                    DELETE ttSubjectColumn.
                END. /* do idex */
            END. /* columns */
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
             WHERE ttSubjectColumn.subjectID EQ ttSubject.subjectID
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
    DEFINE BUFFER bttSubjectColumn   FOR ttSubjectColumn.
    DEFINE BUFFER bttSubjectParamSet FOR ttSubjectParamSet.
    
    OS-CREATE-DIR VALUE("users/" + USERID("ASI")).
    OS-CREATE-DIR VALUE("users/" + USERID("ASI") + "/Subject").
    OUTPUT TO VALUE("users/" + USERID("ASI") + "/Subject/dynSubject.d").
    FOR EACH bttSubject:
        EXPORT bttSubject.
    END.
    OUTPUT CLOSE.
    OUTPUT TO VALUE("users/" + USERID("ASI") + "/Subject/dynSubjectTable.d").
    FOR EACH bttSubjectTable
        BY bttSubjectTable.subjectID
        BY bttSubjectTable.sortOrder:
        EXPORT bttSubjectTable.
    END.
    OUTPUT CLOSE.
    OUTPUT TO VALUE("users/" + USERID("ASI") + "/Subject/dynSubjectWhere.d").
    FOR EACH bttSubjectWhere
        BY bttSubjectWhere.subjectID
        BY bttSubjectWhere.whereTable
        BY bttSubjectWhere.sortOrder:
        EXPORT bttSubjectWhere.
    END.
    OUTPUT CLOSE.
    OUTPUT TO VALUE("users/" + USERID("ASI") + "/Subject/dynSubjectColumn.d").
    FOR EACH bttSubjectColumn
        BY bttSubjectColumn.subjectID
        BY bttSubjectColumn.sortOrder:
        EXPORT bttSubjectColumn.
    END.
    OUTPUT CLOSE.
    OUTPUT TO VALUE("users/" + USERID("ASI") + "/Subject/dynSubjectParamSet.d").
    FOR EACH bttSubjectParamSet
        BY bttSubjectParamSet.subjectID
        BY bttSubjectParamSet.paramSetID:
        EXPORT bttSubjectParamSet.
    END.
    OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetDynParamValue C-Win 
PROCEDURE pSetDynParamValue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
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
                dynParamValue.paramDescription = "System Default"
                .
            FOR EACH ttSubjectParamSet NO-LOCK
                WHERE ttSubjectParamSet.subjectID EQ ttSubject.subjectID,
                EACH dynParamSetDtl NO-LOCK
                WHERE dynParamSetDtl.paramSetID EQ ttSubjectParamSet.paramSetID,
                FIRST dynParam NO-LOCK
                WHERE dynParam.paramID EQ dynParamSetDtl.paramID
                :
                ASSIGN
                    idx                              = idx + 1
                    dynParamValue.paramName[idx]     = dynParamSetDtl.paramName
                    dynParamValue.paramLabel[idx]    = dynParamSetDtl.paramLabel
                    dynParamValue.paramValue[idx]    = dynParamSetDtl.initialValue
                    dynParamValue.paramDataType[idx] = dynParam.dataType
                    dynParamValue.paramFormat[idx]   = dynParam.paramFormat
                    .
            END. /* each dynsubjectparamset */
        END. /* not avail */
        FIND CURRENT dynParamValue NO-LOCK.
    END. /* do trans */

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
            RECT-SAVE:COL = {&COL} + 11
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
    DEFINE BUFFER bttSubjectTable  FOR ttSubjectTable.
    DEFINE BUFFER bttSubjectWhere  FOR ttSubjectWhere.
    
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN tableList.
    END. /* do with */
    CASE subjectSection:
        WHEN "Columns" THEN DO:
            {AOA/includes/pSetOrder.i "ttSubjectColumn" "subjectColumnBrowse"}
        END. /* columns */
        WHEN "Table" THEN DO:
            {AOA/includes/pSetOrder.i "ttSubjectTable" "subjectTableBrowse"}
        END. /* table */
        WHEN "Where" OR WHEN "Parameters" THEN DO:
            {AOA/includes/pSetOrder.i "ttSubjectWhere" "subjectWhereBrowse"}
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
            BROWSE subjectTableBrowse:WIDTH     = FRAME {&FRAME-NAME}:WIDTH
                                                - BROWSE subjectTableBrowse:COL
            BROWSE subjectWhereBrowse:HEIGHT    = cParameter:ROW
                                                - BROWSE subjectWhereBrowse:ROW - .20
            BROWSE subjectColumnBrowse:HEIGHT   = BROWSE subjectBrowse:HEIGHT
            BROWSE subjectColumnBrowse:WIDTH    = FRAME {&FRAME-NAME}:WIDTH
                                                - BROWSE subjectColumnBrowse:COL
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
            btnCloseResults:COL = FRAME resultsFrame:WIDTH - btnCloseResults:WIDTH
            btnSaveResults:COL  = btnCloseResults:COL - btnSaveResults:WIDTH
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
            RECT-PANEL-2:COL    = FRAME paramFrame:WIDTH - RECT-PANEL-2:WIDTH
            btnRunResults:COL   = RECT-PANEL-2:COL + 1
            btnCSV:COL          = btnRunResults:COL + btnRunResults:WIDTH
            btnXLS:COL          = btnCSV:COL + btnCSV:WIDTH
            btnDOCX:COL         = btnXLS:COL + btnXLS:WIDTH
            btnPDF:COL          = btnDOCX:COL + btnDOCX:WIDTH
            btnHTML:COL         = btnPDF:COL + btnPDF:WIDTH
            btnView:COL         = btnHTML:COL + btnHTML:WIDTH
            btnCloseParam:COL   = RECT-PANEL-2:COL - 5
            FRAME frameShow:COL = FRAME paramFrame:WIDTH - FRAME frameShow:WIDTH
            .
    END. /* do with */
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

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
    
    DEFINE BUFFER ttSubjectTable  FOR ttSubjectTable.
    DEFINE BUFFER ttSubjectWhere  FOR ttSubjectWhere.
    DEFINE BUFFER ttSubjectColumn FOR ttSubjectColumn.
    
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
    /*
    FOR EACH ttSubjectColumn
        WHERE ttSubjectColumn.subjectID EQ ttSubject.subjectID
          AND ttSubjectColumn.sortCol   GT 0
           BY ttSubjectColumn.sortCol
        :
        cQueryStr = cQueryStr + " BY " + ttSubjectColumn.fieldName.
    END. /* each ttSubjectColumn */
    */
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fSetShowAll C-Win 
FUNCTION fSetShowAll RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DO WITH FRAME frameShow:
        svShowAll = svShowReportHeader AND
                    svShowParameters   AND
                    svShowPageHeader   AND
                    svShowGroupHeader  AND
                    svShowGroupFooter  AND
                    svShowPageFooter   AND
                    svShowReportFooter
                    .
        DISPLAY {&showFields}.
    END. /* do with */
    RETURN TRUE.

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

