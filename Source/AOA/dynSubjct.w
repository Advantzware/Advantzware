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
&Scoped-define defaultUser _default
&Scoped-define displayFields dynSubject.subjectID ~{&FIELDS-IN-QUERY-viewFrame}
&Scoped-define enabledFields ~{&FIELDS-IN-QUERY-viewFrame}
&Scoped-define transPanel btnUpdate btnAdd btnCopy btnDelete btnReset btnCancel btnCloseView
&Scoped-define transInit btnUpdate btnAdd btnCopy btnDelete btnCloseView
&Scoped-define transUpdate btnUpdate btnReset btnCancel
&Scoped-define showFields svShowAll svShowReportHeader svShowPageHeader ~
svShowGroupHeader svShowGroupFooter svShowPageFooter svShowReportFooter ~
svShowParameters
/* used by rstark to access subjectID 0 by changing value to GE */
&Scoped-define GT GT

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cDataType          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFormat            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLabel             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMode              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPoolName          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrgmName          AS CHARACTER NO-UNDO INITIAL "{&program-id}".
DEFINE VARIABLE hAppSrvBin         AS HANDLE    NO-UNDO.
DEFINE VARIABLE hJasper            AS HANDLE    NO-UNDO.
DEFINE VARIABLE hParamBldr         AS HANDLE    NO-UNDO.
DEFINE VARIABLE hPgmMstrSecur      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hQueryBrowse       AS HANDLE    NO-UNDO.
DEFINE VARIABLE i                  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCopySubjectID     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iOrder             AS INTEGER   NO-UNDO.
DEFINE VARIABLE iUserSecurityLevel AS INTEGER   NO-UNDO.
DEFINE VARIABLE hSection           AS HANDLE    NO-UNDO.
DEFINE VARIABLE lBusinessLogic     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lContinue          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lRefresh           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lSave              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lSuperAdmin        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE rRowID             AS ROWID     NO-UNDO.
DEFINE VARIABLE subjectSection     AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{methods/defines/sortByDefs.i}

DEFINE TEMP-TABLE ttTable NO-UNDO
    FIELD tableName     AS CHARACTER FORMAT "x(20)" LABEL "Table"
    FIELD tableDB       AS CHARACTER FORMAT "x(10)" LABEL "Database"
    FIELD tableDscr     AS CHARACTER FORMAT "x(50)" LABEL "Table Description"
    FIELD businessLogic AS LOGICAL
        INDEX ttTableName IS PRIMARY tableName
        INDEX ttTableDB tableDB
        .
DEFINE TEMP-TABLE ttField NO-UNDO
    FIELD fieldName  AS CHARACTER FORMAT "x(31)" LABEL "Field Name"
    FIELD fieldLabel AS CHARACTER FORMAT "x(31)" LABEL "Field Label"
    FIELD tableDB    AS CHARACTER
        INDEX ttFieldName IS PRIMARY fieldName
        .
DEFINE TEMP-TABLE ttSubjectTable    NO-UNDO LIKE dynSubjectTable
    FIELD tableRowID AS ROWID.
DEFINE TEMP-TABLE ttSubjectWhere    NO-UNDO LIKE dynSubjectWhere
    FIELD tableRowID AS ROWID.
DEFINE TEMP-TABLE ttSubjectColumn   NO-UNDO LIKE dynSubjectColumn
    FIELD tableRowID AS ROWID.
DEFINE TEMP-TABLE ttSubjectParamSet NO-UNDO LIKE dynSubjectParamSet
    FIELD tableRowID AS ROWID.
{AOA/tempTable/ttGroupCalc.i}
{AOA/tempTable/ttDynAction.i}

RUN AOA/appServer/aoaBin.p PERSISTENT SET hAppSrvBin.
SESSION:ADD-SUPER-PROCEDURE (hAppSrvBin).
RUN AOA/spJasper.p PERSISTENT SET hJasper.
SESSION:ADD-SUPER-PROCEDURE (hJasper).
RUN system/PgmMstrSecur.p PERSISTENT SET hPgmMstrSecur.
RUN epCanAccessUser IN hPgmMstrSecur (
    "AOA/dynSubjct.w",
    "SuperAdmin",
    USERID("ASI"),
    OUTPUT lSuperAdmin
    ).
DELETE PROCEDURE hPgmMstrSecur.

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
&Scoped-define INTERNAL-TABLES ttField dynParamSet dynSubject ~
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
&Scoped-define FIELDS-IN-QUERY-subjectBrowse dynSubject.subjectID dynSubject.subjectTitle dynSubject.isActive dynSubject.subjectType dynSubject.module dynSubject.isLookup dynSubject.user-id dynSubject.securityLevel dynSubject.outputFormat dynSubject.recordLimit dynSubject.runSync dynSubject.custListID dynSubject.lastRunDateTime dynSubject.externalForm dynSubject.businessLogic   
&Scoped-define ENABLED-FIELDS-IN-QUERY-subjectBrowse   
&Scoped-define SELF-NAME subjectBrowse
&Scoped-define QUERY-STRING-subjectBrowse FOR EACH dynSubject NO-LOCK WHERE dynSubject.subjectID {&GT} 0   AND dynSubject.securityLevel LE iUserSecurityLevel   AND ((subjectMatches EQ NO  AND dynSubject.subjectTitle BEGINS subjectSearch)    OR  (subjectMatches EQ YES AND dynSubject.subjectTitle MATCHES "*" + subjectSearch + "*"))  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-subjectBrowse OPEN QUERY {&SELF-NAME} FOR EACH dynSubject NO-LOCK WHERE dynSubject.subjectID {&GT} 0   AND dynSubject.securityLevel LE iUserSecurityLevel   AND ((subjectMatches EQ NO  AND dynSubject.subjectTitle BEGINS subjectSearch)    OR  (subjectMatches EQ YES AND dynSubject.subjectTitle MATCHES "*" + subjectSearch + "*"))  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-subjectBrowse dynSubject
&Scoped-define FIRST-TABLE-IN-QUERY-subjectBrowse dynSubject


/* Definitions for BROWSE subjectColumnBrowse                           */
&Scoped-define FIELDS-IN-QUERY-subjectColumnBrowse ttSubjectColumn.sortOrder ttSubjectColumn.isActive ttSubjectColumn.fieldName ttSubjectColumn.fieldLabel ttSubjectColumn.sortCol ttSubjectColumn.sortDescending ttSubjectColumn.isCalcField ttSubjectColumn.isGroup ttSubjectColumn.groupLabel ttSubjectColumn.fieldFormat ttSubjectColumn.isReturnValue ttSubjectColumn.isSearchable ttSubjectColumn.isSortable ttSubjectColumn.isStatusField ttSubjectColumn.statusCompare DROP-DOWN-LIST ttSubjectColumn.compareValue ttSubjectColumn.textColor Blue",1,"Dark Green",2,"Dark Cyan",3,"Dark Red",4,"Dark Purple",5,"Dark Yellow",6,"Dark Gray",7,"Light Gray",8,"Light Blue",9,"Light Green",10,"Light Cyan",11,"Light Red",12,"Light Purple",13,"Light Yellow",14,"White",15 DROP-DOWN-LIST ttSubjectColumn.cellColor Blue",1,"Dark Green",2,"Dark Cyan",3,"Dark Red",4,"Dark Purple",5,"Dark Yellow",6,"Dark Gray",7,"Light Gray",8,"Light Blue",9,"Light Green",10,"Light Cyan",11,"Light Red",12,"Light Purple",13,"Light Yellow",14,"White",15 DROP-DOWN-LIST ttSubjectColumn.statusAction Color Selectable","Cell Color Unselectable","Row Color Selectable","Row Color Unselectable" DROP-DOWN-LIST ttSubjectColumn.custListField ttSubjectColumn.calcProc ttSubjectColumn.calcParam ttSubjectColumn.groupCalc ttSubjectColumn.calcFormula   
&Scoped-define ENABLED-FIELDS-IN-QUERY-subjectColumnBrowse ttSubjectColumn.isActive ttSubjectColumn.fieldLabel ttSubjectColumn.sortCol ttSubjectColumn.sortDescending ttSubjectColumn.isGroup ttSubjectColumn.groupLabel ttSubjectColumn.fieldFormat ttSubjectColumn.isReturnValue ttSubjectColumn.isSearchable ttSubjectColumn.isSortable ttSubjectColumn.isStatusField ttSubjectColumn.statusCompare ttSubjectColumn.compareValue ttSubjectColumn.textColor ttSubjectColumn.cellColor ttSubjectColumn.statusAction ttSubjectColumn.custListField   
&Scoped-define ENABLED-TABLES-IN-QUERY-subjectColumnBrowse ttSubjectColumn
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-subjectColumnBrowse ttSubjectColumn
&Scoped-define SELF-NAME subjectColumnBrowse
&Scoped-define QUERY-STRING-subjectColumnBrowse FOR EACH ttSubjectColumn WHERE ttSubjectColumn.subjectID EQ dynSubject.subjectID   AND ((columnMatches EQ NO AND ttSubjectColumn.fieldName BEGINS columnSearch)    OR (columnMatches EQ YES AND ttSubjectColumn.fieldName MATCHES "*" + columnSearch + "*")) BY ttSubjectColumn.sortOrder
&Scoped-define OPEN-QUERY-subjectColumnBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttSubjectColumn WHERE ttSubjectColumn.subjectID EQ dynSubject.subjectID   AND ((columnMatches EQ NO AND ttSubjectColumn.fieldName BEGINS columnSearch)    OR (columnMatches EQ YES AND ttSubjectColumn.fieldName MATCHES "*" + columnSearch + "*")) BY ttSubjectColumn.sortOrder.
&Scoped-define TABLES-IN-QUERY-subjectColumnBrowse ttSubjectColumn
&Scoped-define FIRST-TABLE-IN-QUERY-subjectColumnBrowse ttSubjectColumn


/* Definitions for BROWSE subjectParamSetBrowse                         */
&Scoped-define FIELDS-IN-QUERY-subjectParamSetBrowse ttSubjectParamSet.sortOrder dynParamSet.setName dynParamSet.setTitle ttSubjectParamSet.isVisible ttSubjectParamSet.useInTitle ttSubjectParamSet.paramSetID ttSubjectParamSet.setRow ttSubjectParamSet.setCol   
&Scoped-define ENABLED-FIELDS-IN-QUERY-subjectParamSetBrowse ttSubjectParamSet.isVisible ttSubjectParamSet.useInTitle   
&Scoped-define ENABLED-TABLES-IN-QUERY-subjectParamSetBrowse ~
ttSubjectParamSet
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-subjectParamSetBrowse ttSubjectParamSet
&Scoped-define SELF-NAME subjectParamSetBrowse
&Scoped-define QUERY-STRING-subjectParamSetBrowse FOR EACH ttSubjectParamSet WHERE ttSubjectParamSet.subjectID EQ dynSubject.subjectID, ~
       FIRST dynParamSet NO-LOCK WHERE dynParamSet.paramSetID EQ ttSubjectParamSet.paramSetID BY ttSubjectParamSet.sortOrder
&Scoped-define OPEN-QUERY-subjectParamSetBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttSubjectParamSet WHERE ttSubjectParamSet.subjectID EQ dynSubject.subjectID, ~
       FIRST dynParamSet NO-LOCK WHERE dynParamSet.paramSetID EQ ttSubjectParamSet.paramSetID BY ttSubjectParamSet.sortOrder.
&Scoped-define TABLES-IN-QUERY-subjectParamSetBrowse ttSubjectParamSet ~
dynParamSet
&Scoped-define FIRST-TABLE-IN-QUERY-subjectParamSetBrowse ttSubjectParamSet
&Scoped-define SECOND-TABLE-IN-QUERY-subjectParamSetBrowse dynParamSet


/* Definitions for BROWSE subjectTableBrowse                            */
&Scoped-define FIELDS-IN-QUERY-subjectTableBrowse ttSubjectTable.tableFind ttSubjectTable.tableName ttSubjectTable.useIndex ttSubjectTable.tableDscr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-subjectTableBrowse   
&Scoped-define SELF-NAME subjectTableBrowse
&Scoped-define QUERY-STRING-subjectTableBrowse FOR EACH ttSubjectTable WHERE ttSubjectTable.subjectID EQ dynSubject.subjectID BY ttSubjectTable.sortOrder
&Scoped-define OPEN-QUERY-subjectTableBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttSubjectTable WHERE ttSubjectTable.subjectID EQ dynSubject.subjectID BY ttSubjectTable.sortOrder.
&Scoped-define TABLES-IN-QUERY-subjectTableBrowse ttSubjectTable
&Scoped-define FIRST-TABLE-IN-QUERY-subjectTableBrowse ttSubjectTable


/* Definitions for BROWSE subjectWhereBrowse                            */
&Scoped-define FIELDS-IN-QUERY-subjectWhereBrowse ttSubjectWhere.sortOrder ttSubjectWhere.whereElement ttSubjectWhere.fieldLabel ttSubjectWhere.dataType ttSubjectWhere.isCalcField ttSubjectWhere.calcProc ttSubjectWhere.calcParam   
&Scoped-define ENABLED-FIELDS-IN-QUERY-subjectWhereBrowse   
&Scoped-define SELF-NAME subjectWhereBrowse
&Scoped-define QUERY-STRING-subjectWhereBrowse FOR EACH ttSubjectWhere WHERE ttSubjectWhere.subjectID  EQ dynSubject.subjectID   AND ttSubjectWhere.whereTable EQ tableList    BY ttSubjectWhere.sortOrder
&Scoped-define OPEN-QUERY-subjectWhereBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttSubjectWhere WHERE ttSubjectWhere.subjectID  EQ dynSubject.subjectID   AND ttSubjectWhere.whereTable EQ tableList    BY ttSubjectWhere.sortOrder.
&Scoped-define TABLES-IN-QUERY-subjectWhereBrowse ttSubjectWhere
&Scoped-define FIRST-TABLE-IN-QUERY-subjectWhereBrowse ttSubjectWhere


/* Definitions for BROWSE tableBrowse                                   */
&Scoped-define FIELDS-IN-QUERY-tableBrowse ttTable.tableName ttTable.tableDB ttTable.tableDscr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-tableBrowse   
&Scoped-define SELF-NAME tableBrowse
&Scoped-define QUERY-STRING-tableBrowse FOR EACH ttTable WHERE ttTable.businessLogic EQ lBusinessLogic AND (tableMatches EQ NO AND (ttTable.tableName BEGINS tableSearch OR ttTable.tableDscr BEGINS tableSearch)) OR (tableMatches EQ YES AND (ttTable.tableName MATCHES "*" + tableSearch + "*" OR ttTable.tableDscr MATCHES "*" + tableSearch + "*"))  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-tableBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttTable WHERE ttTable.businessLogic EQ lBusinessLogic AND (tableMatches EQ NO AND (ttTable.tableName BEGINS tableSearch OR ttTable.tableDscr BEGINS tableSearch)) OR (tableMatches EQ YES AND (ttTable.tableName MATCHES "*" + tableSearch + "*" OR ttTable.tableDscr MATCHES "*" + tableSearch + "*"))  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-tableBrowse ttTable
&Scoped-define FIRST-TABLE-IN-QUERY-tableBrowse ttTable


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-subjectBrowse}

/* Definitions for FRAME viewFrame                                      */
&Scoped-define FIELDS-IN-QUERY-viewFrame dynSubject.isActive ~
dynSubject.isLookup dynSubject.securityLevel dynSubject.subjectTitle ~
dynSubject.subjectType dynSubject.user-id dynSubject.module ~
dynSubject.mnemonic dynSubject.subjectGroup dynSubject.outputFormat ~
dynSubject.externalForm dynSubject.businessLogic dynSubject.subjectAltID ~
dynSubject.useCustList dynSubject.custListID dynSubject.runSync ~
dynSubject.saveLastRun dynSubject.lastRunDateTime dynSubject.recordLimit 
&Scoped-define ENABLED-FIELDS-IN-QUERY-viewFrame dynSubject.custListID 
&Scoped-define ENABLED-TABLES-IN-QUERY-viewFrame dynSubject
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-viewFrame dynSubject
&Scoped-define QUERY-STRING-viewFrame FOR EACH dynSubject SHARE-LOCK
&Scoped-define OPEN-QUERY-viewFrame OPEN QUERY viewFrame FOR EACH dynSubject SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-viewFrame dynSubject
&Scoped-define FIRST-TABLE-IN-QUERY-viewFrame dynSubject


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnDesign btnResults btnSetInitialize ~
btnUndo subjectTab btnSubjectParamSet paramSetsTab tableTab btnViewSubject ~
whereTab columnsTab btnCreateDefaults btnNow btnOuterJoin btnToday btnTime ~
btnDateTime subjectSearch subjectMatches tableSearch tableMatches ~
btnCalcField tableList btnOF tableListOf btnWhere btnMatches subjectBrowse ~
tableBrowse subjectWhereBrowse btnBegins btnAND btnOR btnEQ btnNE btnLT ~
btnGT fieldSearch fieldMatches paramSetSearch btnErrorCheck paramSetMatches ~
btnLE btnGE paramSetBrowse fieldBrowse btnPlus btnMinus ~
subjectParamSetBrowse btnMultiply btnDivide btnMoveUp btnYes btnNo btnDate ~
cUseIndex findType btnDec subjectTableBrowse btnInt columnSearch btnSave ~
columnMatches btnStr subjectColumnBrowse btnSubstr cParameter btnOpen ~
btnSyntax btnClose cConstant btnPeriod btnDouble btnComma btnSingle ~
btnAddSelections queryStr btnGroupCalc btnAddUseIndex btnRemoveUseIndex ~
btnAddParameter btnRemoveSelection btnMoveDown btnAddConstant btnRemove ~
cParameterLabel cConstantLabel 
&Scoped-Define DISPLAYED-OBJECTS subjectSearch subjectMatches tableSearch ~
tableMatches tableList tableListOf fieldSearch fieldMatches paramSetSearch ~
paramSetMatches cUseIndex findType columnSearch columnMatches cParameter ~
cConstant queryStr subjectSectionLabel paramSetsSectionLabel ~
tableSectionLabel whereSectionLabel columnsSectionLabel cUseIndexLabel ~
cParameterLabel cConstantLabel queryText 

/* Custom List Definitions                                              */
/* allSection,tableSection,whereSection,parameterSection,columnsSection,subjectSection */
&Scoped-define allSection RECT-TABLE RECT-FIELD RECT-QUERYTABLE ~
btnSetInitialize RECT-QUERYSTR RECT-COLUMN btnUndo RECT-PARAM ~
btnSubjectParamSet btnViewSubject btnNow btnOuterJoin btnToday btnTime ~
btnDateTime tableSearch tableMatches btnCalcField tableList btnOF ~
tableListOf btnWhere btnMatches tableBrowse subjectWhereBrowse btnBegins ~
btnAND btnOR btnEQ btnNE btnLT btnGT fieldSearch fieldMatches ~
paramSetSearch paramSetMatches btnLE btnGE paramSetBrowse fieldBrowse ~
btnPlus btnMinus subjectParamSetBrowse btnMultiply btnDivide btnMoveUp ~
btnYes btnNo btnDate cUseIndex findType btnDec subjectTableBrowse btnInt ~
columnSearch btnSave columnMatches btnStr subjectColumnBrowse btnSubstr ~
cParameter btnOpen btnSyntax btnClose cConstant btnPeriod btnDouble ~
btnComma btnSingle btnAddSelections queryStr btnGroupCalc btnAddUseIndex ~
btnRemoveUseIndex btnAddParameter btnRemoveSelection btnMoveDown ~
btnAddConstant btnRemove cUseIndexLabel cParameterLabel cConstantLabel ~
queryText 
&Scoped-define tableSection RECT-TABLE RECT-QUERYTABLE RECT-QUERYSTR ~
tableSearch tableMatches tableBrowse btnMoveUp cUseIndex findType ~
subjectTableBrowse btnSyntax btnAddSelections queryStr btnAddUseIndex ~
btnRemoveUseIndex btnRemoveSelection btnMoveDown btnRemove cUseIndexLabel ~
queryText 
&Scoped-define whereSection RECT-FIELD RECT-QUERYSTR RECT-PARAM btnNow ~
btnOuterJoin btnToday btnTime btnDateTime btnCalcField tableList btnOF ~
tableListOf btnWhere btnMatches subjectWhereBrowse btnBegins btnAND btnOR ~
btnEQ btnNE btnLT btnGT fieldSearch fieldMatches paramSetMatches btnLE ~
btnGE fieldBrowse btnPlus btnMinus btnMultiply btnDivide btnMoveUp btnYes ~
btnNo btnDate btnDec btnInt btnStr btnSubstr cParameter btnOpen btnSyntax ~
btnClose cConstant btnPeriod btnDouble btnComma btnSingle btnAddSelections ~
queryStr btnAddParameter btnRemoveSelection btnMoveDown btnAddConstant ~
btnRemove cParameterLabel cConstantLabel queryText 
&Scoped-define parameterSection btnSetInitialize RECT-PARAM ~
btnSubjectParamSet paramSetSearch paramSetMatches paramSetBrowse ~
subjectParamSetBrowse btnMoveUp btnAddSelections btnRemoveSelection ~
btnMoveDown btnRemove 
&Scoped-define columnsSection RECT-FIELD RECT-COLUMN RECT-PARAM ~
btnCalcField fieldSearch fieldMatches paramSetMatches fieldBrowse btnMoveUp ~
columnSearch columnMatches subjectColumnBrowse btnAddSelections ~
btnGroupCalc btnRemoveSelection btnMoveDown btnRemove 
&Scoped-define subjectSection btnDesign btnResults btnViewSubject ~
btnCreateDefaults subjectSearch subjectMatches 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetUseIndex C-Win 
FUNCTION fGetUseIndex RETURNS CHARACTER
  (ipcTableDB AS CHARACTER, ipcTableName AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetVariable C-Win 
FUNCTION fGetVariable RETURNS CHARACTER
  (ipParamName AS CHARACTER, ipParamDataType AS CHARACTER)  FORWARD.

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

DEFINE BUTTON btnCalcField 
     IMAGE-UP FILE "Graphics/16x16/calculator.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Calculated Field".

DEFINE BUTTON btnClose 
     LABEL ")" 
     SIZE 5 BY 1.05 TOOLTIP "Close Parentheses".

DEFINE BUTTON btnComma 
     LABEL "," 
     SIZE 2.4 BY 1.05 TOOLTIP "Comma".

DEFINE BUTTON btnCreateDefaults 
     LABEL "Create Default/Test" 
     SIZE 23 BY 1 TOOLTIP "Create Defaults".

DEFINE BUTTON btnDate 
     LABEL "DATE (" 
     SIZE 10 BY 1.05 TOOLTIP "Date".

DEFINE BUTTON btnDateTime 
     LABEL "DATETIME (" 
     SIZE 14 BY 1.05 TOOLTIP "DATETIME".

DEFINE BUTTON btnDec 
     LABEL "DEC (" 
     SIZE 10 BY 1.05 TOOLTIP "Decimal".

DEFINE BUTTON btnDesign 
     IMAGE-UP FILE "Graphics/32x32/compasses.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Query Design" 
     SIZE 8 BY 1.91 TOOLTIP "Query Design".

DEFINE BUTTON btnDivide 
     LABEL "/" 
     SIZE 5 BY 1.05 TOOLTIP "Divide".

DEFINE BUTTON btnDouble 
     LABEL "~"" 
     SIZE 2.4 BY 1.05 TOOLTIP "Double Quote".

DEFINE BUTTON btnEQ 
     LABEL "EQ" 
     SIZE 5 BY 1.05 TOOLTIP "Equals".

DEFINE BUTTON btnErrorCheck 
     IMAGE-UP FILE "Graphics/16x16/save.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "E" 
     SIZE 4.4 BY 1.05.

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

DEFINE BUTTON btnResults 
     IMAGE-UP FILE "Graphics/32x32/media_play.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Results" 
     SIZE 8 BY 1.91 TOOLTIP "Run Subject".

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Save" 
     SIZE 8 BY 1.91 TOOLTIP "Save".

DEFINE BUTTON btnSetInitialize  NO-FOCUS FLAT-BUTTON
     LABEL "Initialize" 
     SIZE 9.2 BY 1.91.

DEFINE BUTTON btnSingle 
     LABEL "'" 
     SIZE 2.4 BY 1.05 TOOLTIP "Single Quote".

DEFINE BUTTON btnStr 
     LABEL "STRING (" 
     SIZE 10 BY 1.05 TOOLTIP "String".

DEFINE BUTTON btnSubjectParamSet 
     IMAGE-UP FILE "Graphics/32x32/udf.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Subject Parameter Set Builder" 
     SIZE 8 BY 1.91 TOOLTIP "Subject Parameter Set Builder".

DEFINE BUTTON btnSubstr 
     LABEL "SUBSTR(" 
     SIZE 10 BY 1.05 TOOLTIP "Substring".

DEFINE BUTTON btnSyntax 
     IMAGE-UP FILE "AOA/images/navigate_check.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Syntax" 
     SIZE 4.4 BY 1 TOOLTIP "Check Query Syntax".

DEFINE BUTTON btnTime 
     LABEL "TIME" 
     SIZE 8 BY 1.05 TOOLTIP "TIME".

DEFINE BUTTON btnToday 
     LABEL "TODAY" 
     SIZE 10 BY 1.05 TOOLTIP "TODAY".

DEFINE BUTTON btnUndo 
     IMAGE-UP FILE "Graphics/32x32/undo_32.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Undo" 
     SIZE 8 BY 1.91 TOOLTIP "Undo Changes".

DEFINE BUTTON btnViewSubject 
     IMAGE-UP FILE "Graphics/32x32/udf.png":U NO-FOCUS FLAT-BUTTON
     LABEL "View Subject" 
     SIZE 8 BY 1.91 TOOLTIP "View Subject".

DEFINE BUTTON btnWhere 
     LABEL "WHERE" 
     SIZE 10 BY 1.05 TOOLTIP "WHERE".

DEFINE BUTTON btnYes 
     LABEL "YES" 
     SIZE 5 BY 1.05 TOOLTIP "Yes".

DEFINE VARIABLE cParameter AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX SORT INNER-LINES 100
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

DEFINE VARIABLE columnsSectionLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Columns" 
      VIEW-AS TEXT 
     SIZE 13 BY 1.43
     BGCOLOR 22 FONT 24 NO-UNDO.

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

DEFINE VARIABLE paramSetsSectionLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Parameter Sets" 
      VIEW-AS TEXT 
     SIZE 23 BY 1.43
     BGCOLOR 22 FONT 24 NO-UNDO.

DEFINE VARIABLE queryText AS CHARACTER FORMAT "X(256)":U INITIAL " Query" 
      VIEW-AS TEXT 
     SIZE 8 BY .62
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE VARIABLE subjectSearch AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE subjectSectionLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Subject" 
      VIEW-AS TEXT 
     SIZE 12 BY 1.43
     BGCOLOR 22 FONT 24 NO-UNDO.

DEFINE VARIABLE tableSearch AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE tableSectionLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Table" 
      VIEW-AS TEXT 
     SIZE 8 BY 1.43
     BGCOLOR 22 FONT 24 NO-UNDO.

DEFINE VARIABLE whereSectionLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Where" 
      VIEW-AS TEXT 
     SIZE 10 BY 1.43
     BGCOLOR 22 FONT 24 NO-UNDO.

DEFINE IMAGE columnsTab
     FILENAME "Graphics/32x32/tabdown72.png":U
     STRETCH-TO-FIT
     SIZE 15 BY 1.91.

DEFINE IMAGE paramSetsTab
     FILENAME "Graphics/32x32/tabdown72.png":U
     STRETCH-TO-FIT
     SIZE 25 BY 1.91.

DEFINE IMAGE subjectTab
     FILENAME "Graphics/32x32/tabdown72.png":U
     STRETCH-TO-FIT
     SIZE 14 BY 1.91.

DEFINE IMAGE tableTab
     FILENAME "Graphics/32x32/tabdown72.png":U
     STRETCH-TO-FIT
     SIZE 10 BY 1.91.

DEFINE IMAGE whereTab
     FILENAME "Graphics/32x32/tabdown72.png":U
     STRETCH-TO-FIT
     SIZE 12 BY 1.91.

DEFINE VARIABLE findType AS CHARACTER INITIAL "EACH" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Each", "EACH",
"First", "FIRST",
"Last", "LAST"
     SIZE 28 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 0    
     SIZE 158 BY .48
     BGCOLOR 29 .

DEFINE RECTANGLE RECT-COLUMN
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 78 BY 1.43.

DEFINE RECTANGLE RECT-FIELD
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 36 BY 1.43.

DEFINE RECTANGLE RECT-PARAM
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 36 BY 1.43.

DEFINE RECTANGLE RECT-QUERY
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 37 BY 1.43.

DEFINE RECTANGLE RECT-QUERYSTR
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 83 BY 6.43.

DEFINE RECTANGLE RECT-QUERYTABLE
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 31 BY 1.43.

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

DEFINE BUTTON btnAddEmail 
     IMAGE-UP FILE "AOA/images/navigate_plus.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Email" 
     SIZE 4.4 BY 1.05 TOOLTIP "Add Recipents".

DEFINE BUTTON btnCSV 
     IMAGE-UP FILE "Graphics/32x32/spreadsheet_sum.png":U NO-FOCUS FLAT-BUTTON
     LABEL "csv" 
     SIZE 8 BY 1.91 TOOLTIP "Excel CSV".

DEFINE BUTTON btnDOCX 
     IMAGE-UP FILE "Graphics/32x32/docx.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Word DOCX".

DEFINE BUTTON btnHTML 
     IMAGE-UP FILE "Graphics/32x32/html_tag.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "HTML".

DEFINE BUTTON btnLocalCSV 
     IMAGE-UP FILE "Graphics/32x32/csv.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Local Excel CSV".

DEFINE BUTTON btnPageFormat 
     IMAGE-UP FILE "Graphics/32x32/document_gear.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Page Format".

DEFINE BUTTON btnPDF 
     IMAGE-UP FILE "Graphics/32x32/pdf.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "PDF".

DEFINE BUTTON btnPrint 
     IMAGE-UP FILE "Graphics/32x32/print_new.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Printer".

DEFINE BUTTON btnRunResults 
     IMAGE-UP FILE "Graphics/32x32/table.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Results Grid" 
     SIZE 8 BY 1.91 TOOLTIP "Results Grid".

DEFINE BUTTON btnView 
     IMAGE-UP FILE "Graphics/32x32/jss_icon.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Jasper Viewer".

DEFINE BUTTON btnXLS 
     IMAGE-UP FILE "Graphics/32x32/xls.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Excel XLS".

DEFINE VARIABLE svRecipients AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 68 BY 1.67
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-PANEL-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 82 BY 2.38.

DEFINE RECTANGLE RECT-SHOW
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 157 BY 1.19.

DEFINE VARIABLE svRunSync AS LOGICAL INITIAL no 
     LABEL "Run Synchronous" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

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

DEFINE BUTTON btnCloseResults 
     IMAGE-UP FILE "AOA/images/navigate_cross.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Close Results" 
     SIZE 4.4 BY 1 TOOLTIP "Close Results".

DEFINE BUTTON btnSaveResults 
     IMAGE-UP FILE "AOA/images/navigate_check.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Save Results" 
     SIZE 4.4 BY 1 TOOLTIP "Save Results".

DEFINE BUTTON btnAdd 
     IMAGE-UP FILE "Graphics/32x32/navigate_plus.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_plus_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Add" 
     SIZE 8 BY 1.91 TOOLTIP "Add".

DEFINE BUTTON btnCancel 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_cross_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91 TOOLTIP "Cancel".

DEFINE BUTTON btnCloseView 
     IMAGE-UP FILE "Graphics/16x16/delete.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Close" 
     SIZE 4.2 BY 1 TOOLTIP "Close".

DEFINE BUTTON btnCopy 
     IMAGE-UP FILE "Graphics/32x32/element_copy.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/element_copy_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Copy" 
     SIZE 8 BY 1.91 TOOLTIP "Copy".

DEFINE BUTTON btnDelete 
     IMAGE-UP FILE "Graphics/32x32/garbage_can.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/garbage_can_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Delete" 
     SIZE 8 BY 1.91 TOOLTIP "Delete".

DEFINE BUTTON btnPageFormat-2 
     IMAGE-UP FILE "Graphics/32x32/document_gear.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Page Format".

DEFINE BUTTON btnReset 
     IMAGE-UP FILE "Graphics/32x32/undo_32.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/undo_32_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91 TOOLTIP "Reset".

DEFINE BUTTON btnUpdate 
     IMAGE-UP FILE "Graphics/32x32/pencil.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/pencil_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Update" 
     SIZE 8 BY 1.91 TOOLTIP "Update/Save".

DEFINE RECTANGLE RECT-PANEL
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 58 BY 2.38
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fieldBrowse FOR 
      ttField SCROLLING.

DEFINE QUERY paramSetBrowse FOR 
      dynParamSet SCROLLING.

DEFINE QUERY subjectBrowse FOR 
      dynSubject SCROLLING.

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

DEFINE QUERY viewFrame FOR 
      dynSubject SCROLLING.
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
      dynParamSet.setName LABEL-BGCOLOR 14
dynParamSet.paramSetID LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 36 BY 4.19
         TITLE "Available Parameter Sets".

DEFINE BROWSE subjectBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS subjectBrowse C-Win _FREEFORM
  QUERY subjectBrowse DISPLAY
      dynSubject.subjectID LABEL-BGCOLOR 14
dynSubject.subjectTitle LABEL-BGCOLOR 14
dynSubject.isActive VIEW-AS TOGGLE-BOX
dynSubject.subjectType LABEL-BGCOLOR 14
dynSubject.module LABEL-BGCOLOR 14
dynSubject.isLookup VIEW-AS TOGGLE-BOX
dynSubject.user-id
dynSubject.securityLevel
dynSubject.outputFormat
dynSubject.recordLimit
dynSubject.runSync
dynSubject.custListID
dynSubject.lastRunDateTime
dynSubject.externalForm
dynSubject.businessLogic
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 37 BY 4.19
         TITLE "Subject" ROW-HEIGHT-CHARS .62.

DEFINE BROWSE subjectColumnBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS subjectColumnBrowse C-Win _FREEFORM
  QUERY subjectColumnBrowse DISPLAY
      ttSubjectColumn.sortOrder
ttSubjectColumn.isActive VIEW-AS TOGGLE-BOX
ttSubjectColumn.fieldName FORMAT "x(40)"
ttSubjectColumn.fieldLabel
ttSubjectColumn.sortCol
ttSubjectColumn.sortDescending VIEW-AS TOGGLE-BOX
ttSubjectColumn.isCalcField VIEW-AS TOGGLE-BOX
ttSubjectColumn.isGroup VIEW-AS TOGGLE-BOX
ttSubjectColumn.groupLabel
ttSubjectColumn.fieldFormat
ttSubjectColumn.isReturnValue VIEW-AS TOGGLE-BOX
ttSubjectColumn.isSearchable VIEW-AS TOGGLE-BOX
ttSubjectColumn.isSortable VIEW-AS TOGGLE-BOX
ttSubjectColumn.isStatusField VIEW-AS TOGGLE-BOX
ttSubjectColumn.statusCompare VIEW-AS COMBO-BOX INNER-LINES 8 LIST-ITEMS ",EQ,NE,LT,LE,GT,GE,BEGINS" DROP-DOWN-LIST
ttSubjectColumn.compareValue
ttSubjectColumn.textColor VIEW-AS COMBO-BOX INNER-LINES 16 LIST-ITEM-PAIRS "Black",0,"Dark Blue",1,"Dark Green",2,"Dark Cyan",3,"Dark Red",4,"Dark Purple",5,"Dark Yellow",6,"Dark Gray",7,"Light Gray",8,"Light Blue",9,"Light Green",10,"Light Cyan",11,"Light Red",12,"Light Purple",13,"Light Yellow",14,"White",15 DROP-DOWN-LIST
ttSubjectColumn.cellColor VIEW-AS COMBO-BOX INNER-LINES 16 LIST-ITEM-PAIRS "Black",0,"Dark Blue",1,"Dark Green",2,"Dark Cyan",3,"Dark Red",4,"Dark Purple",5,"Dark Yellow",6,"Dark Gray",7,"Light Gray",8,"Light Blue",9,"Light Green",10,"Light Cyan",11,"Light Red",12,"Light Purple",13,"Light Yellow",14,"White",15 DROP-DOWN-LIST
ttSubjectColumn.statusAction VIEW-AS COMBO-BOX INNER-LINES 5 LIST-ITEMS "","Cell Color Selectable","Cell Color Unselectable","Row Color Selectable","Row Color Unselectable" DROP-DOWN-LIST
ttSubjectColumn.custListField VIEW-AS TOGGLE-BOX
ttSubjectColumn.calcProc
ttSubjectColumn.calcParam
ttSubjectColumn.groupCalc
ttSubjectColumn.calcFormula
ENABLE
ttSubjectColumn.isActive
ttSubjectColumn.fieldLabel
ttSubjectColumn.sortCol
ttSubjectColumn.sortDescending
ttSubjectColumn.isGroup
ttSubjectColumn.groupLabel
ttSubjectColumn.fieldFormat
ttSubjectColumn.isReturnValue
ttSubjectColumn.isSearchable
ttSubjectColumn.isSortable
ttSubjectColumn.isStatusField
ttSubjectColumn.statusCompare
ttSubjectColumn.compareValue
ttSubjectColumn.textColor
ttSubjectColumn.cellColor
ttSubjectColumn.statusAction
ttSubjectColumn.custListField
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS MULTIPLE SIZE 78 BY 4.19
         TITLE "Subject Columns" ROW-HEIGHT-CHARS .62.

DEFINE BROWSE subjectParamSetBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS subjectParamSetBrowse C-Win _FREEFORM
  QUERY subjectParamSetBrowse DISPLAY
      ttSubjectParamSet.sortOrder
dynParamSet.setName
dynParamSet.setTitle
ttSubjectParamSet.isVisible VIEW-AS TOGGLE-BOX
ttSubjectParamSet.useInTitle VIEW-AS TOGGLE-BOX
ttSubjectParamSet.paramSetID
ttSubjectParamSet.setRow
ttSubjectParamSet.setCol
ENABLE
ttSubjectParamSet.isVisible
ttSubjectParamSet.useInTitle
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 78 BY 4.19
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
      ttSubjectWhere.sortOrder
ttSubjectWhere.whereElement
ttSubjectWhere.fieldLabel
ttSubjectWhere.dataType
ttSubjectWhere.isCalcField VIEW-AS TOGGLE-BOX
ttSubjectWhere.calcProc
ttSubjectWhere.calcParam
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
     btnDesign AT ROW 1 COL 88 HELP
          "Query Design" WIDGET-ID 292
     btnResults AT ROW 1 COL 80 HELP
          "Run Subject" WIDGET-ID 250
     btnSetInitialize AT ROW 1 COL 104 WIDGET-ID 296
     btnUndo AT ROW 1 COL 121 HELP
          "Undo Changes" WIDGET-ID 282
     btnSubjectParamSet AT ROW 1 COL 96 HELP
          "Subject Parameter Set Builder" WIDGET-ID 286
     btnViewSubject AT ROW 1 COL 96 HELP
          "Access View Subject" WIDGET-ID 288
     btnCreateDefaults AT ROW 1 COL 138 HELP
          "Create Defaults" WIDGET-ID 278
     btnNow AT ROW 1.71 COL 138 WIDGET-ID 194
     btnOuterJoin AT ROW 1.71 COL 146 WIDGET-ID 274
     btnToday AT ROW 2.91 COL 128 WIDGET-ID 190
     btnTime AT ROW 2.91 COL 138 WIDGET-ID 192
     btnDateTime AT ROW 2.91 COL 146 WIDGET-ID 196
     subjectSearch AT ROW 3.86 COL 3 NO-LABEL WIDGET-ID 16
     subjectMatches AT ROW 3.86 COL 26 HELP
          "Select for Matches Search" WIDGET-ID 38
     tableSearch AT ROW 3.86 COL 41 NO-LABEL WIDGET-ID 2
     tableMatches AT ROW 3.86 COL 64 HELP
          "Select for Table Search Matches" WIDGET-ID 40
     btnCalcField AT ROW 18.38 COL 77 HELP
          "Calculated Field" WIDGET-ID 280
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
     fieldMatches AT ROW 9.81 COL 64 HELP
          "Select for Table Search Matches" WIDGET-ID 52
     paramSetSearch AT ROW 10.05 COL 83 HELP
          "Enter Field Search" NO-LABEL WIDGET-ID 258
     btnErrorCheck AT ROW 3.86 COL 77 WIDGET-ID 294
     paramSetMatches AT ROW 10.05 COL 106 HELP
          "Select for Table Search Matches" WIDGET-ID 256
     btnLE AT ROW 10.05 COL 150 WIDGET-ID 76
     btnGE AT ROW 10.05 COL 155 WIDGET-ID 78
     paramSetBrowse AT ROW 11 COL 2 WIDGET-ID 1000
     fieldBrowse AT ROW 11 COL 40 WIDGET-ID 700
     btnPlus AT ROW 11.24 COL 150 WIDGET-ID 158
     btnMinus AT ROW 11.24 COL 155 WIDGET-ID 156
     subjectParamSetBrowse AT ROW 12.43 COL 82 WIDGET-ID 1100
     btnMultiply AT ROW 12.43 COL 150 WIDGET-ID 160
     btnDivide AT ROW 12.43 COL 155 WIDGET-ID 154
     btnMoveUp AT ROW 9.81 COL 77 HELP
          "Move Up" WIDGET-ID 64
     btnYes AT ROW 13.62 COL 150 WIDGET-ID 172
     btnNo AT ROW 13.62 COL 155 WIDGET-ID 174
     btnDate AT ROW 14.81 COL 150 WIDGET-ID 166
     cUseIndex AT ROW 15.52 COL 12 COLON-ALIGNED NO-LABEL WIDGET-ID 262
     findType AT ROW 15.52 COL 50 NO-LABEL WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160.2 BY 28.57
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     btnDec AT ROW 16 COL 150 WIDGET-ID 164
     subjectTableBrowse AT ROW 16.95 COL 82 WIDGET-ID 400
     btnInt AT ROW 17.19 COL 150 WIDGET-ID 162
     columnSearch AT ROW 17.43 COL 2 HELP
          "Enter Column Search" NO-LABEL WIDGET-ID 112
     btnSave AT ROW 1 COL 113 HELP
          "Update/Save" WIDGET-ID 248
     columnMatches AT ROW 17.43 COL 67 HELP
          "Select for Column Search Matches" WIDGET-ID 110
     btnStr AT ROW 18.38 COL 150 WIDGET-ID 168
     subjectColumnBrowse AT ROW 18.62 COL 1 WIDGET-ID 900
     btnSubstr AT ROW 19.57 COL 150 WIDGET-ID 170
     cParameter AT ROW 20.76 COL 91 COLON-ALIGNED HELP
          "Select Parameter Type" NO-LABEL WIDGET-ID 204
     btnOpen AT ROW 20.76 COL 150 WIDGET-ID 94
     btnSyntax AT ROW 23.86 COL 78 WIDGET-ID 202
     btnClose AT ROW 20.76 COL 155 WIDGET-ID 96
     cConstant AT ROW 21.95 COL 91 COLON-ALIGNED NO-LABEL WIDGET-ID 176
     btnPeriod AT ROW 21.95 COL 150 WIDGET-ID 236
     btnDouble AT ROW 21.95 COL 152.4 WIDGET-ID 240
     btnComma AT ROW 21.95 COL 155 WIDGET-ID 242
     btnSingle AT ROW 21.95 COL 157.4 WIDGET-ID 244
     btnAddSelections AT ROW 7.91 COL 77 HELP
          "Add Selections" WIDGET-ID 200
     queryStr AT ROW 23.86 COL 83 NO-LABEL WIDGET-ID 4
     btnGroupCalc AT ROW 16.48 COL 77 HELP
          "Group Calculations" WIDGET-ID 272
     btnAddUseIndex AT ROW 15.52 COL 38 WIDGET-ID 268
     btnRemoveUseIndex AT ROW 15.52 COL 43 WIDGET-ID 270
     btnAddParameter AT ROW 20.76 COL 145 WIDGET-ID 208
     btnRemoveSelection AT ROW 14.1 COL 77 HELP
          "Remove Selections" WIDGET-ID 198
     btnMoveDown AT ROW 12.19 COL 77 HELP
          "Move Down" WIDGET-ID 62
     btnAddConstant AT ROW 21.95 COL 145 WIDGET-ID 180
     btnRemove AT ROW 11 COL 77 HELP
          "Remove" WIDGET-ID 66
     subjectSectionLabel AT ROW 1.48 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 312
     paramSetsSectionLabel AT ROW 1.48 COL 15.5 COLON-ALIGNED NO-LABEL WIDGET-ID 314
     tableSectionLabel AT ROW 1.48 COL 41 COLON-ALIGNED NO-LABEL WIDGET-ID 316
     whereSectionLabel AT ROW 1.48 COL 51.5 COLON-ALIGNED NO-LABEL WIDGET-ID 318
     columnsSectionLabel AT ROW 1.48 COL 64 COLON-ALIGNED NO-LABEL WIDGET-ID 320
     cUseIndexLabel AT ROW 15.52 COL 2 NO-LABEL WIDGET-ID 266
     cParameterLabel AT ROW 20.76 COL 80 COLON-ALIGNED NO-LABEL WIDGET-ID 206
     cConstantLabel AT ROW 21.95 COL 81 COLON-ALIGNED NO-LABEL WIDGET-ID 178
     queryText AT ROW 22.91 COL 78 COLON-ALIGNED NO-LABEL WIDGET-ID 108
     RECT-QUERY AT ROW 3.62 COL 2 WIDGET-ID 44
     RECT-TABLE AT ROW 3.62 COL 40 WIDGET-ID 46
     RECT-FIELD AT ROW 9.57 COL 40 WIDGET-ID 48
     RECT-QUERYTABLE AT ROW 15.29 COL 48 WIDGET-ID 54
     RECT-QUERYSTR AT ROW 23.14 COL 77 WIDGET-ID 58
     RECT-COLUMN AT ROW 17.19 COL 1 WIDGET-ID 114
     RECT-PARAM AT ROW 9.81 COL 82 WIDGET-ID 260
     subjectTab AT ROW 1.24 COL 2 WIDGET-ID 302
     paramSetsTab AT ROW 1.24 COL 16.5 WIDGET-ID 304
     tableTab AT ROW 1.24 COL 42 WIDGET-ID 306
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160.2 BY 28.57
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     whereTab AT ROW 1.24 COL 52.5 WIDGET-ID 308
     columnsTab AT ROW 1.24 COL 65 WIDGET-ID 310
     RECT-1 AT ROW 2.91 COL 2 WIDGET-ID 322
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160.2 BY 28.57
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.

DEFINE FRAME paramFrame
     btnCloseParam AT ROW 1 COL 156 HELP
          "Jasper Viewer" WIDGET-ID 252
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 23.14
         SIZE 160 BY 6.43
         FGCOLOR 1  WIDGET-ID 1300.

DEFINE FRAME outputFrame
     btnCSV AT ROW 1.48 COL 94 HELP
          "Excel CSV" WIDGET-ID 140
     btnLocalCSV AT ROW 1.48 COL 86 HELP
          "Local Excel CSV" WIDGET-ID 656
     svRecipients AT ROW 1.24 COL 8 NO-LABEL WIDGET-ID 600
     svRunSync AT ROW 2.91 COL 8 HELP
          "Toggle to Run Synchronous" WIDGET-ID 654
     svShowAll AT ROW 4.1 COL 8 WIDGET-ID 18
     svShowReportHeader AT ROW 4.1 COL 24 WIDGET-ID 2
     svShowReportFooter AT ROW 4.1 COL 45 WIDGET-ID 4
     btnPageFormat AT ROW 1.48 COL 142 HELP
          "Page Format" WIDGET-ID 652
     svShowPageHeader AT ROW 4.1 COL 66 WIDGET-ID 6
     svShowPageFooter AT ROW 4.1 COL 85 WIDGET-ID 8
     svShowGroupHeader AT ROW 4.1 COL 104 WIDGET-ID 10
     svShowGroupFooter AT ROW 4.1 COL 124 WIDGET-ID 12
     svShowParameters AT ROW 4.1 COL 143 WIDGET-ID 16
     btnPrint AT ROW 1.48 COL 134 HELP
          "Printer" WIDGET-ID 644
     btnRunResults AT ROW 1.48 COL 78 HELP
          "Results Grid" WIDGET-ID 254
     btnAddEmail AT ROW 1.95 COL 3 HELP
          "Add Recipents" WIDGET-ID 636
     btnDOCX AT ROW 1.48 COL 110 HELP
          "Word DOCX" WIDGET-ID 142
     btnHTML AT ROW 1.48 COL 126 HELP
          "HTML" WIDGET-ID 144
     btnPDF AT ROW 1.48 COL 118 HELP
          "PDF" WIDGET-ID 146
     btnView AT ROW 1.48 COL 150 HELP
          "Jasper Viewer" WIDGET-ID 148
     btnXLS AT ROW 1.48 COL 102 HELP
          "Excel XLS" WIDGET-ID 150
     "Email:" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.24 COL 2 WIDGET-ID 640
     RECT-PANEL-2 AT ROW 1.24 COL 77 WIDGET-ID 256
     RECT-SHOW AT ROW 3.86 COL 2 WIDGET-ID 642
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 159 BY 5.24
         BGCOLOR 21 FGCOLOR 15 
         TITLE BGCOLOR 15 "Parameters" WIDGET-ID 1400.

DEFINE FRAME resultsFrame
     btnCloseResults AT ROW 1 COL 6 HELP
          "Jasper Viewer" WIDGET-ID 252
     btnSaveResults AT ROW 1 COL 2 HELP
          "Jasper Viewer" WIDGET-ID 254
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 139 ROW 9.81
         SIZE 10 BY 2.38
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 1200.

DEFINE FRAME viewFrame
     btnDelete AT ROW 13.95 COL 47 HELP
          "Delete" WIDGET-ID 124
     dynSubject.subjectID AT ROW 1.24 COL 16 COLON-ALIGNED WIDGET-ID 140
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
          BGCOLOR 15 
     dynSubject.isActive AT ROW 1.24 COL 34 WIDGET-ID 148
          VIEW-AS TOGGLE-BOX
          SIZE 10 BY 1
     dynSubject.isLookup AT ROW 1.24 COL 45 WIDGET-ID 166
          VIEW-AS TOGGLE-BOX
          SIZE 11 BY 1
     dynSubject.securityLevel AT ROW 1.24 COL 70 COLON-ALIGNED WIDGET-ID 138
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 
     dynSubject.subjectTitle AT ROW 2.43 COL 16 COLON-ALIGNED WIDGET-ID 142
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
          BGCOLOR 15 
     dynSubject.subjectType AT ROW 2.43 COL 70 COLON-ALIGNED WIDGET-ID 146
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "System","User" 
          DROP-DOWN-LIST
          SIZE 16 BY 1
     dynSubject.user-id AT ROW 3.62 COL 16 COLON-ALIGNED WIDGET-ID 144
          VIEW-AS FILL-IN 
          SIZE 16.2 BY 1
          BGCOLOR 15 
     dynSubject.module AT ROW 3.62 COL 45 COLON-ALIGNED WIDGET-ID 160
          VIEW-AS COMBO-BOX INNER-LINES 20
          LIST-ITEMS "","AP","AR","DC","EQ","FG","GL","HS","JC","NS","OE","PO","RM","SB","SS","TS" 
          DROP-DOWN-LIST
          SIZE 8.2 BY 1
     dynSubject.mnemonic AT ROW 3.62 COL 77 COLON-ALIGNED WIDGET-ID 668
          LABEL "Hotkey"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
          BGCOLOR 15 
     dynSubject.subjectGroup AT ROW 4.81 COL 16 COLON-ALIGNED WIDGET-ID 672
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "Item 1" 
          DROP-DOWN-LIST
          SIZE 37 BY 1
     dynSubject.outputFormat AT ROW 6 COL 18 NO-LABEL WIDGET-ID 150
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Grid", "Grid":U,
"Local CSV", "LocalCSV":U,
"CSV", "CSV":U,
"XLS", "XLS":U,
"DocX", "DocX":U,
"PDF", "PDF":U,
"HTML", "HTML":U
          SIZE 70 BY 1
     dynSubject.externalForm AT ROW 7.19 COL 16 COLON-ALIGNED WIDGET-ID 134 FORMAT "x(80)"
          VIEW-AS FILL-IN 
          SIZE 70 BY 1
          BGCOLOR 15 
     dynSubject.businessLogic AT ROW 8.38 COL 16 COLON-ALIGNED WIDGET-ID 132 FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 70 BY 1
          BGCOLOR 15 
     dynSubject.subjectAltID AT ROW 9.57 COL 16 COLON-ALIGNED WIDGET-ID 654
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
          BGCOLOR 15 
     dynSubject.useCustList AT ROW 9.57 COL 46 WIDGET-ID 664
          LABEL "Use -->"
          VIEW-AS TOGGLE-BOX
          SIZE 11 BY 1
     dynSubject.custListID AT ROW 9.57 COL 73 COLON-ALIGNED WIDGET-ID 666
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "Item 1" 
          DROP-DOWN-LIST
          SIZE 13 BY 1
     dynSubject.runSync AT ROW 10.76 COL 18 WIDGET-ID 656
          LABEL "Run Synchronous"
          VIEW-AS TOGGLE-BOX
          SIZE 21 BY 1
     dynSubject.saveLastRun AT ROW 10.76 COL 46 WIDGET-ID 670
          VIEW-AS TOGGLE-BOX
          SIZE 36 BY 1
     dynSubject.lastRunDateTime AT ROW 11.95 COL 16 COLON-ALIGNED WIDGET-ID 164
          LABEL "Last Run"
          VIEW-AS FILL-IN 
          SIZE 34.2 BY 1
          BGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 73 ROW 13.38
         SIZE 88 BY 16.19
         FGCOLOR 1  WIDGET-ID 1500.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME viewFrame
     dynSubject.recordLimit AT ROW 11.95 COL 70 COLON-ALIGNED WIDGET-ID 162
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     btnCopy AT ROW 13.95 COL 39 HELP
          "Copy" WIDGET-ID 122
     btnCancel AT ROW 13.95 COL 63 HELP
          "Cancel" WIDGET-ID 120
     btnPageFormat-2 AT ROW 13.91 COL 71 HELP
          "Page Format" WIDGET-ID 652
     btnCloseView AT ROW 1.24 COL 84 HELP
          "Close" WIDGET-ID 72
     btnUpdate AT ROW 13.95 COL 23 HELP
          "Update/Save" WIDGET-ID 128
     btnAdd AT ROW 13.95 COL 31 HELP
          "Add" WIDGET-ID 118
     btnReset AT ROW 13.95 COL 55 HELP
          "Reset" WIDGET-ID 126
     "Output Default:" VIEW-AS TEXT
          SIZE 15 BY 1 AT ROW 6 COL 2 WIDGET-ID 158
     RECT-PANEL AT ROW 13.62 COL 22 WIDGET-ID 130
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 73 ROW 13.38
         SIZE 88 BY 16.19
         FGCOLOR 1 
         TITLE "View" WIDGET-ID 1500.


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
         WIDTH              = 160.2
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics/32x32/jss_icon_32.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics/32x32/jss_icon_32.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME outputFrame:FRAME = FRAME paramFrame:HANDLE
       FRAME paramFrame:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME resultsFrame:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME viewFrame:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB subjectBrowse btnMatches DEFAULT-FRAME */
/* BROWSE-TAB tableBrowse subjectBrowse DEFAULT-FRAME */
/* BROWSE-TAB subjectWhereBrowse tableBrowse DEFAULT-FRAME */
/* BROWSE-TAB paramSetBrowse btnGE DEFAULT-FRAME */
/* BROWSE-TAB fieldBrowse paramSetBrowse DEFAULT-FRAME */
/* BROWSE-TAB subjectParamSetBrowse btnMinus DEFAULT-FRAME */
/* BROWSE-TAB subjectTableBrowse btnDec DEFAULT-FRAME */
/* BROWSE-TAB subjectColumnBrowse btnStr DEFAULT-FRAME */
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

/* SETTINGS FOR BUTTON btnCalcField IN FRAME DEFAULT-FRAME
   1 3 5                                                                */
ASSIGN 
       btnCalcField:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnClose IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnClose:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnComma IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnComma:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnCreateDefaults IN FRAME DEFAULT-FRAME
   6                                                                    */
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

/* SETTINGS FOR BUTTON btnDesign IN FRAME DEFAULT-FRAME
   6                                                                    */
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

/* SETTINGS FOR BUTTON btnResults IN FRAME DEFAULT-FRAME
   6                                                                    */
/* SETTINGS FOR BUTTON btnSave IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnSetInitialize IN FRAME DEFAULT-FRAME
   1 4                                                                  */
/* SETTINGS FOR BUTTON btnSingle IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnSingle:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnStr IN FRAME DEFAULT-FRAME
   1 3                                                                  */
ASSIGN 
       btnStr:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnSubjectParamSet IN FRAME DEFAULT-FRAME
   1 4                                                                  */
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

/* SETTINGS FOR BUTTON btnUndo IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON btnViewSubject IN FRAME DEFAULT-FRAME
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
   ALIGN-L 1 5                                                          */
ASSIGN 
       columnSearch:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN columnsSectionLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       columnsTab:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "Columns".

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
       paramSetBrowse:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       paramSetBrowse:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR TOGGLE-BOX paramSetMatches IN FRAME DEFAULT-FRAME
   1 3 4 5                                                              */
ASSIGN 
       paramSetMatches:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN paramSetSearch IN FRAME DEFAULT-FRAME
   ALIGN-L 1 4                                                          */
ASSIGN 
       paramSetSearch:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN paramSetsSectionLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       paramSetsTab:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "Parameters".

/* SETTINGS FOR EDITOR queryStr IN FRAME DEFAULT-FRAME
   1 2 3                                                                */
ASSIGN 
       queryStr:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE
       queryStr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN queryText IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 2 3                                                      */
ASSIGN 
       queryText:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-COLUMN IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 5                                                        */
ASSIGN 
       RECT-COLUMN:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-FIELD IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 3 5                                                      */
ASSIGN 
       RECT-FIELD:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-PARAM IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 3 4 5                                                    */
ASSIGN 
       RECT-PARAM:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-QUERY IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-QUERYSTR IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 2 3                                                      */
ASSIGN 
       RECT-QUERYSTR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-QUERYTABLE IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 2                                                        */
ASSIGN 
       RECT-QUERYTABLE:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-TABLE IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 2                                                        */
ASSIGN 
       RECT-TABLE:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       subjectBrowse:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 3
       subjectBrowse:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR BROWSE subjectColumnBrowse IN FRAME DEFAULT-FRAME
   1 5                                                                  */
ASSIGN 
       subjectColumnBrowse:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       subjectColumnBrowse:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 1.

/* SETTINGS FOR TOGGLE-BOX subjectMatches IN FRAME DEFAULT-FRAME
   6                                                                    */
/* SETTINGS FOR BROWSE subjectParamSetBrowse IN FRAME DEFAULT-FRAME
   1 4                                                                  */
ASSIGN 
       subjectParamSetBrowse:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

/* SETTINGS FOR FILL-IN subjectSearch IN FRAME DEFAULT-FRAME
   ALIGN-L 6                                                            */
/* SETTINGS FOR FILL-IN subjectSectionLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       subjectTab:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "Subject".

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

/* SETTINGS FOR FILL-IN tableSectionLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tableTab:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "Table".

/* SETTINGS FOR FILL-IN whereSectionLabel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       whereTab:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "Where".

/* SETTINGS FOR FRAME outputFrame
                                                                        */
ASSIGN 
       btnPageFormat:AUTO-RESIZE IN FRAME outputFrame      = TRUE.

ASSIGN 
       btnPrint:AUTO-RESIZE IN FRAME outputFrame      = TRUE.

ASSIGN 
       btnView:AUTO-RESIZE IN FRAME outputFrame      = TRUE.

/* SETTINGS FOR RECTANGLE RECT-PANEL-2 IN FRAME outputFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-SHOW IN FRAME outputFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME paramFrame
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME paramFrame:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME resultsFrame
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME resultsFrame:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME viewFrame
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME viewFrame:HIDDEN           = TRUE
       FRAME viewFrame:MOVABLE          = TRUE.

/* SETTINGS FOR BUTTON btnCancel IN FRAME viewFrame
   NO-ENABLE                                                            */
ASSIGN 
       btnPageFormat-2:AUTO-RESIZE IN FRAME viewFrame      = TRUE.

/* SETTINGS FOR BUTTON btnReset IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dynSubject.businessLogic IN FRAME viewFrame
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN dynSubject.externalForm IN FRAME viewFrame
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR TOGGLE-BOX dynSubject.isActive IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX dynSubject.isLookup IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dynSubject.lastRunDateTime IN FRAME viewFrame
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN dynSubject.mnemonic IN FRAME viewFrame
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR COMBO-BOX dynSubject.module IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET dynSubject.outputFormat IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dynSubject.recordLimit IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-PANEL IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX dynSubject.runSync IN FRAME viewFrame
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX dynSubject.saveLastRun IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dynSubject.securityLevel IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dynSubject.subjectAltID IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX dynSubject.subjectGroup IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dynSubject.subjectID IN FRAME viewFrame
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN dynSubject.subjectTitle IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX dynSubject.subjectType IN FRAME viewFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX dynSubject.useCustList IN FRAME viewFrame
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN dynSubject.user-id IN FRAME viewFrame
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

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
     _Query            is NOT OPENED
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
     _Query            is NOT OPENED
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
OPEN QUERY {&SELF-NAME} FOR EACH dynSubject NO-LOCK
WHERE dynSubject.subjectID {&GT} 0
  AND dynSubject.securityLevel LE iUserSecurityLevel
  AND ((subjectMatches EQ NO  AND dynSubject.subjectTitle BEGINS subjectSearch)
   OR  (subjectMatches EQ YES AND dynSubject.subjectTitle MATCHES "*" + subjectSearch + "*"))
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE subjectBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE subjectColumnBrowse
/* Query rebuild information for BROWSE subjectColumnBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSubjectColumn
WHERE ttSubjectColumn.subjectID EQ dynSubject.subjectID
  AND ((columnMatches EQ NO AND ttSubjectColumn.fieldName BEGINS columnSearch)
   OR (columnMatches EQ YES AND ttSubjectColumn.fieldName MATCHES "*" + columnSearch + "*"))
BY ttSubjectColumn.sortOrder.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE subjectColumnBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE subjectParamSetBrowse
/* Query rebuild information for BROWSE subjectParamSetBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSubjectParamSet
WHERE ttSubjectParamSet.subjectID EQ dynSubject.subjectID,
FIRST dynParamSet NO-LOCK
WHERE dynParamSet.paramSetID EQ ttSubjectParamSet.paramSetID
BY ttSubjectParamSet.sortOrder.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE subjectParamSetBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE subjectTableBrowse
/* Query rebuild information for BROWSE subjectTableBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSubjectTable
WHERE ttSubjectTable.subjectID EQ dynSubject.subjectID
BY ttSubjectTable.sortOrder.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE subjectTableBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE subjectWhereBrowse
/* Query rebuild information for BROWSE subjectWhereBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSubjectWhere
WHERE ttSubjectWhere.subjectID  EQ dynSubject.subjectID
  AND ttSubjectWhere.whereTable EQ tableList
   BY ttSubjectWhere.sortOrder.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE subjectWhereBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE tableBrowse
/* Query rebuild information for BROWSE tableBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttTable
WHERE ttTable.businessLogic EQ lBusinessLogic
AND (tableMatches EQ NO
AND (ttTable.tableName BEGINS tableSearch
OR ttTable.tableDscr BEGINS tableSearch))
OR (tableMatches EQ YES
AND (ttTable.tableName MATCHES "*" + tableSearch + "*"
OR ttTable.tableDscr MATCHES "*" + tableSearch + "*"))
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE tableBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME viewFrame
/* Query rebuild information for FRAME viewFrame
     _TblList          = "ASI.dynSubject"
     _Query            is NOT OPENED
*/  /* FRAME viewFrame */
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


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd C-Win
ON CHOOSE OF btnAdd IN FRAME viewFrame /* Add */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnAddConstant
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddConstant C-Win
ON CHOOSE OF btnAddConstant IN FRAME DEFAULT-FRAME /* Add Constant */
DO:
    ASSIGN cConstant.
    RUN pAddWhere (tableList, cConstant, "").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME outputFrame
&Scoped-define SELF-NAME btnAddEmail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddEmail C-Win
ON CHOOSE OF btnAddEmail IN FRAME outputFrame /* Email */
DO:
    RUN pRecipients (svRecipients:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
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


&Scoped-define SELF-NAME btnCalcField
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalcField C-Win
ON CHOOSE OF btnCalcField IN FRAME DEFAULT-FRAME
DO:
    RUN pCalculatedField.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME viewFrame /* Cancel */
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
    IF AVAILABLE dynSubject THEN
    BROWSE subjectBrowse:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME resultsFrame
&Scoped-define SELF-NAME btnCloseResults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCloseResults C-Win
ON CHOOSE OF btnCloseResults IN FRAME resultsFrame /* Close Results */
DO:
    FRAME resultsFrame:HIDDEN = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnCloseView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCloseView C-Win
ON CHOOSE OF btnCloseView IN FRAME viewFrame /* Close */
DO:
    SELF:MOVE-TO-BOTTOM().
    FRAME viewFrame:HIDDEN = YES.
    DO WITH FRAME DEFAULT-FRAME:
        ASSIGN
            btnCreateDefaults:SENSITIVE = YES
            btnResults:SENSITIVE        = YES
            subjectSearch:SENSITIVE     = YES
            subjectMatches:SENSITIVE    = YES
            .
    END. /* with frame */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCopy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopy C-Win
ON CHOOSE OF btnCopy IN FRAME viewFrame /* Copy */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnCreateDefaults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCreateDefaults C-Win
ON CHOOSE OF btnCreateDefaults IN FRAME DEFAULT-FRAME /* Create Default/Test */
DO:
    DO TRANSACTION:
        FIND FIRST dynParamValue EXCLUSIVE-LOCK
             WHERE dynParamValue.subjectID    EQ dynSubject.subjectID
               AND dynParamValue.user-id      EQ "{&defaultUser}"
               AND dynParamValue.prgmName     EQ "{&program-id}"
               AND dynParamValue.paramValueID EQ 0
             NO-ERROR.
        IF AVAILABLE dynParamValue THEN
        DELETE dynParamValue.
    END. /* do trans */
    RUN pCreateDynParameters (FRAME paramFrame:HANDLE, YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCreateDefaults C-Win
ON RIGHT-MOUSE-CLICK OF btnCreateDefaults IN FRAME DEFAULT-FRAME /* Create Default/Test */
DO:
    RUN AOA/dynImportExport.w (OUTPUT lRefresh).
    IF lRefresh THEN DO:
        SESSION:SET-WAIT-STATE("General").
        RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
        RUN pLoadSubject.
        RUN LockWindowUpdate (0,OUTPUT i).
        SESSION:SET-WAIT-STATE("").
    END. /* if refresh */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME outputFrame
&Scoped-define SELF-NAME btnCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCSV C-Win
ON CHOOSE OF btnCSV IN FRAME outputFrame /* csv */
DO:
    RUN pRunSubject (YES, "CSV", "{&defaultUser}", cPrgmName).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete C-Win
ON CHOOSE OF btnDelete IN FRAME viewFrame /* Delete */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnDesign
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDesign C-Win
ON CHOOSE OF btnDesign IN FRAME DEFAULT-FRAME /* Query Design */
DO:
    RUN pQueryDesign.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME outputFrame
&Scoped-define SELF-NAME btnDOCX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDOCX C-Win
ON CHOOSE OF btnDOCX IN FRAME outputFrame
DO:
    RUN pRunSubject (YES, "DOCX", "{&defaultUser}", cPrgmName).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnErrorCheck
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnErrorCheck C-Win
ON CHOOSE OF btnErrorCheck IN FRAME DEFAULT-FRAME /* E */
DO:
    RUN pErrorCheck.
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


&Scoped-define FRAME-NAME outputFrame
&Scoped-define SELF-NAME btnHTML
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHTML C-Win
ON CHOOSE OF btnHTML IN FRAME outputFrame
DO:
    RUN pRunSubject (YES, "HTML", "{&defaultUser}", cPrgmName).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLocalCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLocalCSV C-Win
ON CHOOSE OF btnLocalCSV IN FRAME outputFrame
DO:
    RUN pRunSubject (YES, "LocalCSV", "{&defaultUser}", cPrgmName).
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


&Scoped-define FRAME-NAME outputFrame
&Scoped-define SELF-NAME btnPageFormat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPageFormat C-Win
ON CHOOSE OF btnPageFormat IN FRAME outputFrame
DO:
    RUN pPageFormat.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnPageFormat-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPageFormat-2 C-Win
ON CHOOSE OF btnPageFormat-2 IN FRAME viewFrame
DO:
    RUN pPageFormat.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME outputFrame
&Scoped-define SELF-NAME btnPDF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPDF C-Win
ON CHOOSE OF btnPDF IN FRAME outputFrame
DO:
    RUN pRunSubject (YES, "PDF", "{&defaultUser}", cPrgmName).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrint C-Win
ON CHOOSE OF btnPrint IN FRAME outputFrame
DO:
    RUN pRunSubject (YES, "Print -d", "{&defaultUser}", cPrgmName).
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
        WHEN "Parameters" THEN
            IF AVAILABLE ttSubjectParamSet THEN
            APPLY "DEFAULT-ACTION":U TO subjectParamSetBrowse.
        WHEN "Table" THEN
            IF AVAILABLE ttSubjectTable THEN
            APPLY "DEFAULT-ACTION":U TO subjectTableBrowse.
        WHEN "Where" THEN
            IF AVAILABLE ttSubjectWhere THEN
            APPLY "DEFAULT-ACTION":U TO subjectWhereBrowse.
    END CASE.
    fSetSaveButton (YES).
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


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset C-Win
ON CHOOSE OF btnReset IN FRAME viewFrame /* Reset */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnResults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnResults C-Win
ON CHOOSE OF btnResults IN FRAME DEFAULT-FRAME /* Results */
DO:
    RUN pGetDynParamValue (dynSubject.subjectID, "{&defaultUser}", cPrgmName, 0).
    IF AVAILABLE dynParamValue THEN
    RUN AOA/dynRun.w PERSISTENT (cPrgmName, ROWID(dynParamValue), YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnResults C-Win
ON RIGHT-MOUSE-CLICK OF btnResults IN FRAME DEFAULT-FRAME /* Results */
DO:
    MESSAGE 
        "Delete Non-Default Lookups?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    UPDATE lDeleteLookups AS LOGICAL.
    IF lDeleteLookups THEN DO TRANSACTION:
        FOR EACH dynParamValue EXCLUSIVE-LOCK
            WHERE dynParamValue.user-id NE "_default"
              AND dynParamValue.isLookup EQ YES
            :
            DELETE dynParamValue.
        END. /* each dynparamvalue */
        MESSAGE 
            "Done."
        VIEW-AS ALERT-BOX.
    END. /* do trans */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME outputFrame
&Scoped-define SELF-NAME btnRunResults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRunResults C-Win
ON CHOOSE OF btnRunResults IN FRAME outputFrame /* Results Grid */
DO:
    RUN pRunSubject (YES, "Grid", "{&defaultUser}", cPrgmName).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* Save */
DO:
    fSetSaveButton (NO).
    ASSIGN
        BROWSE subjectColumnBrowse:MODIFIED   = NO
        BROWSE subjectParamSetBrowse:MODIFIED = NO
        .
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
&Scoped-define SELF-NAME btnSetInitialize
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSetInitialize C-Win
ON CHOOSE OF btnSetInitialize IN FRAME DEFAULT-FRAME /* Initialize */
DO:
    IF AVAILABLE dynSubject THEN
    RUN AOA/dynPageParam.w ("", 0, dynSubject.subjectID).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSubjectParamSet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSubjectParamSet C-Win
ON CHOOSE OF btnSubjectParamSet IN FRAME DEFAULT-FRAME /* Subject Parameter Set Builder */
DO:
    IF NOT VALID-HANDLE(hParamBldr) THEN
    RUN AOA/subjectSetBldr.w PERSISTENT SET hParamBldr (
        THIS-PROCEDURE,
        dynSubject.subjectID
        ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSubjectParamSet C-Win
ON RIGHT-MOUSE-CLICK OF btnSubjectParamSet IN FRAME DEFAULT-FRAME /* Subject Parameter Set Builder */
DO:
    FIND FIRST dynParamValue NO-LOCK
         WHERE dynParamValue.subjectID    EQ dynSubject.subjectID
           AND dynParamValue.user-id      EQ "{&defaultUser}"
           AND dynParamValue.paramValueID EQ 0
         NO-ERROR.
    IF AVAILABLE dynParamValue THEN
    RUN pGenerateDefsInclude.
    ELSE
    MESSAGE
        "Default Dynamic Parameter Value record not yet created!"
    VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSyntax
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSyntax C-Win
ON CHOOSE OF btnSyntax IN FRAME DEFAULT-FRAME /* Syntax */
DO:
    RUN pRunSubject (NO, ?, "{&defaultUser}", cPrgmName).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUndo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUndo C-Win
ON CHOOSE OF btnUndo IN FRAME DEFAULT-FRAME /* Undo */
DO:
    fSetSaveButton (NO).
    BROWSE subjectColumnBrowse:MODIFIED = NO.
    RUN pLoadSubject.
    APPLY "VALUE-CHANGED":U TO subjectBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME btnUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUpdate C-Win
ON CHOOSE OF btnUpdate IN FRAME viewFrame /* Update */
DO:
    RUN pCRUD (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME outputFrame
&Scoped-define SELF-NAME btnView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnView C-Win
ON CHOOSE OF btnView IN FRAME outputFrame
DO:
    RUN pRunSubject (YES, "View", "{&defaultUser}", cPrgmName).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnViewSubject
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnViewSubject C-Win
ON CHOOSE OF btnViewSubject IN FRAME DEFAULT-FRAME /* View Subject */
DO:
    APPLY "DEFAULT-ACTION":U TO subjectBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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


&Scoped-define FRAME-NAME outputFrame
&Scoped-define SELF-NAME btnXLS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnXLS C-Win
ON CHOOSE OF btnXLS IN FRAME outputFrame
DO:
    RUN pRunSubject (YES, "XLS", "{&defaultUser}", cPrgmName).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME dynSubject.businessLogic
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dynSubject.businessLogic C-Win
ON HELP OF dynSubject.businessLogic IN FRAME viewFrame /* Business Logic */
DO:
    DEFINE VARIABLE cBaseDir AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFile    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cInitDir AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lOK      AS LOGICAL   NO-UNDO.

    ASSIGN
        cBaseDir = REPLACE(SEARCH("copyrite"),"copyrite","")
        cInitDir = cBaseDir + "AOA/dynBL"
        .
    SYSTEM-DIALOG GET-FILE cFile
        TITLE "Select BL File"
        FILTERS "Source Files (*.p)" "*.p",
                "R-Code Files (*.r)" "*.r",
                "All Files    (*.*) " "*.*"
        INITIAL-DIR cInitDir
        RETURN-TO-START-DIR
        MUST-EXIST
        USE-FILENAME
        UPDATE lOK
        .
    IF lOK THEN
    ASSIGN
        cFile = REPLACE(cFile,cBaseDir,"")
        SELF:SCREEN-VALUE = REPLACE(cFile,".r",".p")
        .
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


&Scoped-define SELF-NAME columnsTab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL columnsTab C-Win
ON MOUSE-SELECT-CLICK OF columnsTab IN FRAME DEFAULT-FRAME
DO:
    RUN pSection (SELF).
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


&Scoped-define FRAME-NAME viewFrame
&Scoped-define SELF-NAME dynSubject.externalForm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dynSubject.externalForm C-Win
ON HELP OF dynSubject.externalForm IN FRAME viewFrame /* External Form */
DO:
    DEFINE VARIABLE cJrxmlFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cInitDir   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lOK        AS LOGICAL   NO-UNDO.

    cInitDir = ".\".
    SYSTEM-DIALOG GET-FILE cJrxmlFile
        TITLE "Select Image File"
        FILTERS "Jasper Report Files (*.jrxml)" "*.jrxml",
                "All Files (*.*) " "*.*"
        INITIAL-DIR cInitDir
        MUST-EXIST
        USE-FILENAME
        UPDATE lOK
        .
    IF lOK THEN
    SELF:SCREEN-VALUE = cJrxmlFile.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME fieldBrowse
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME fieldBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fieldBrowse C-Win
ON DEFAULT-ACTION OF fieldBrowse IN FRAME DEFAULT-FRAME /* Available Fields */
DO:
    fSetSaveButton (YES).
    CASE subjectSection:
        WHEN "Columns" THEN
        RUN pAddColumn (ttField.fieldName).
        WHEN "Table" THEN
        RUN pAddTable (ttTable.tableDB, ttTable.tableName, ttTable.tableDscr).
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
    {AOA/includes/startSearch.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fieldBrowse C-Win
ON VALUE-CHANGED OF fieldBrowse IN FRAME DEFAULT-FRAME /* Available Fields */
DO:
    IF AVAILABLE ttField THEN
    SELF:TOOLTIP = ttField.fieldLabel + " (" + ttField.fieldName + ")".
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
        fSetSaveButton (YES).
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL paramSetBrowse C-Win
ON START-SEARCH OF paramSetBrowse IN FRAME DEFAULT-FRAME /* Available Parameter Sets */
DO:
    {AOA/includes/startSearch.i}
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


&Scoped-define SELF-NAME paramSetsTab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL paramSetsTab C-Win
ON MOUSE-SELECT-CLICK OF paramSetsTab IN FRAME DEFAULT-FRAME
DO:
    RUN pSection (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME subjectBrowse
&Scoped-define SELF-NAME subjectBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL subjectBrowse C-Win
ON DEFAULT-ACTION OF subjectBrowse IN FRAME DEFAULT-FRAME /* Subject */
DO:
    IF subjectSection EQ "Subject" THEN DO:
        IF lSave THEN
        APPLY "CHOOSE":U TO btnSave.
        ASSIGN
            btnCreateDefaults:SENSITIVE = NO
            btnResults:SENSITIVE        = NO
            subjectSearch:SENSITIVE     = NO
            subjectMatches:SENSITIVE    = NO
            .
        VIEW FRAME viewFrame.
        RUN pDisplay.
    END. /* if subject */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL subjectBrowse C-Win
ON START-SEARCH OF subjectBrowse IN FRAME DEFAULT-FRAME /* Subject */
DO:
    &Scoped-define startSearchValueChanged
    {AOA/includes/startSearch.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL subjectBrowse C-Win
ON VALUE-CHANGED OF subjectBrowse IN FRAME DEFAULT-FRAME /* Subject */
DO:
    ASSIGN
        lBusinessLogic     = dynSubject.businessLogic NE ""
        btnDesign:HIDDEN   = lBusinessLogic
        .
    IF lBusinessLogic THEN
    RUN pGetBusinessLogicTable (dynSubject.businessLogic).
    {&OPEN-QUERY-tableBrowse}
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
    IF NOT FRAME viewFrame:HIDDEN THEN
    RUN pDisplay.
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL subjectColumnBrowse C-Win
ON ROW-LEAVE OF subjectColumnBrowse IN FRAME DEFAULT-FRAME /* Subject Columns */
DO:
    IF BROWSE subjectColumnBrowse:MODIFIED THEN
    fSetSaveButton (YES).
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
    RUN pSetOrder.
    {&OPEN-QUERY-subjectParamSetBrowse}
    RUN pGetParamList.
    fSetSaveButton (YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL subjectParamSetBrowse C-Win
ON ROW-LEAVE OF subjectParamSetBrowse IN FRAME DEFAULT-FRAME /* Subject Parameter Sets */
DO:
    IF BROWSE subjectParamSetBrowse:MODIFIED THEN
    fSetSaveButton (YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME subjectSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL subjectSearch C-Win
ON LEAVE OF subjectSearch IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-subjectBrowse}
    IF AVAILABLE dynSubject THEN
    APPLY "VALUE-CHANGED":U TO subjectBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL subjectSearch C-Win
ON RETURN OF subjectSearch IN FRAME DEFAULT-FRAME
DO:
    APPLY "LEAVE":U TO SELF.
    APPLY "ENTRY":U TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME subjectTab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL subjectTab C-Win
ON MOUSE-SELECT-CLICK OF subjectTab IN FRAME DEFAULT-FRAME
DO:
    RUN pSection (SELF).
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
        cUseIndex:LIST-ITEMS  = ?
        .
    IF ttTable.businessLogic EQ NO THEN
    ASSIGN
        cUseIndex:LIST-ITEMS   = fGetUseIndex(ttSubjectTable.tableDB,ttSubjectTable.tableName)
        cUseIndex:SCREEN-VALUE = cUseIndex:ENTRY(1)
        cUseIndex:INNER-LINES  = cUseIndex:NUM-ITEMS
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


&Scoped-define FRAME-NAME outputFrame
&Scoped-define SELF-NAME svShowAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowAll C-Win
ON VALUE-CHANGED OF svShowAll IN FRAME outputFrame /* Show ALL */
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
  DISPLAY {&showFields} WITH FRAME outputFrame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowGroupFooter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowGroupFooter C-Win
ON VALUE-CHANGED OF svShowGroupFooter IN FRAME outputFrame /* Group Footer */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowGroupHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowGroupHeader C-Win
ON VALUE-CHANGED OF svShowGroupHeader IN FRAME outputFrame /* Group Header */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowPageFooter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowPageFooter C-Win
ON VALUE-CHANGED OF svShowPageFooter IN FRAME outputFrame /* Page Footer */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowPageHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowPageHeader C-Win
ON VALUE-CHANGED OF svShowPageHeader IN FRAME outputFrame /* Page Header */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowParameters
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowParameters C-Win
ON VALUE-CHANGED OF svShowParameters IN FRAME outputFrame /* Parameters */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowReportFooter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowReportFooter C-Win
ON VALUE-CHANGED OF svShowReportFooter IN FRAME outputFrame /* Report Footer */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowReportHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowReportHeader C-Win
ON VALUE-CHANGED OF svShowReportHeader IN FRAME outputFrame /* Report Header */
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
    {AOA/includes/startSearch.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tableBrowse C-Win
ON VALUE-CHANGED OF tableBrowse IN FRAME DEFAULT-FRAME /* Available Tables */
DO:
    IF AVAILABLE ttTable THEN
    SELF:TOOLTIP = ttTable.tableDscr + " (" + ttTable.tableName + ")".
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


&Scoped-define SELF-NAME tableTab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tableTab C-Win
ON MOUSE-SELECT-CLICK OF tableTab IN FRAME DEFAULT-FRAME
DO:
    RUN pSection (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME whereTab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL whereTab C-Win
ON MOUSE-SELECT-CLICK OF whereTab IN FRAME DEFAULT-FRAME
DO:
    RUN pSection (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME fieldBrowse
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

{methods/menus/stdHelpMenu.i}
&Scoped-define sdBrowseName subjectBrowse
{methods/template/brwcustom2.i 1}
&Scoped-define sdBrowseName paramSetBrowse
{methods/template/brwcustom2.i 2}
&Scoped-define sdBrowseName subjectParamSetBrowse
{methods/template/brwcustom2.i 3}
&Scoped-define sdBrowseName tableBrowse
{methods/template/brwcustom2.i 4}
&Scoped-define sdBrowseName subjectTableBrowse
{methods/template/brwcustom2.i 5}
&Scoped-define sdBrowseName fieldBrowse
{methods/template/brwcustom2.i 6}
&Scoped-define sdBrowseName subjectWhereBrowse
{methods/template/brwcustom2.i 7}
&Scoped-define sdBrowseName subjectColumnBrowse
{methods/template/brwcustom2.i 8}


/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN pDeleteProcedure.
   RUN disable_UI.
END.

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

{AOA/includes/dynProcs.i "tt"}
{AOA/includes/pGetDynParamValue.i}
{AOA/includes/pJasperGroupCalc.i "tt"}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  IF lContinue THEN DO:
    SESSION:SET-WAIT-STATE("General").
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    RUN pLoadSubject.
    RUN pSetObjects.
    RUN pGetSettings.
    RUN pGetCustListID.
    RUN enable_UI.
    IF AVAILABLE dynSubject THEN DO:
        HIDE {&allSection} IN FRAME {&FRAME-NAME}.
        RUN pGetDBTables.
        APPLY "VALUE-CHANGED":U TO BROWSE subjectBrowse.
        RUN pGetParamList.
    END. /* if avail */
  END. /* if continue */
  hSection = subjectTab:HANDLE.
  RUN pSection (hSection).
  RUN LockWindowUpdate (0,OUTPUT i).
  SESSION:SET-WAIT-STATE("").
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
  IF NOT lContinue THEN
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

&Scoped-define sdBrowseName subjectBrowse
{methods/sortByProc.i "pBySubjectTitle" "dynSubject.subjectTitle"}
{methods/sortByProc.i "pBySubjectID" "dynSubject.subjectID"}
{methods/sortByProc.i "pBySubjectType" "dynSubject.subjectType"}
{methods/sortByProc.i "pByModule" "dynSubject.module"}

&Scoped-define sdBrowseName tableBrowse
{methods/sortByProc.i "pByTableName" "ttTable.tableName"}
{methods/sortByProc.i "pByTableDB" "ttTable.tableDB"}
{methods/sortByProc.i "pByTableDscr" "ttTable.tableDscr"}

&Scoped-define sdBrowseName fieldBrowse
{methods/sortByProc.i "pByFieldName" "ttField.fieldName"}
{methods/sortByProc.i "pByFieldLabel" "ttField.fieldLabel"}

&Scoped-define sdBrowseName paramSetBrowse
{methods/sortByProc.i "pBySetName" "dynParamSet.setName"}
{methods/sortByProc.i "pByParamSetID" "dynParamSet.paramSetID"}

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
  DISPLAY subjectSearch subjectMatches tableSearch tableMatches tableList 
          tableListOf fieldSearch fieldMatches paramSetSearch paramSetMatches 
          cUseIndex findType columnSearch columnMatches cParameter cConstant 
          queryStr subjectSectionLabel paramSetsSectionLabel tableSectionLabel 
          whereSectionLabel columnsSectionLabel cUseIndexLabel cParameterLabel 
          cConstantLabel queryText 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnDesign btnResults btnSetInitialize btnUndo subjectTab 
         btnSubjectParamSet paramSetsTab tableTab btnViewSubject whereTab 
         columnsTab btnCreateDefaults btnNow btnOuterJoin btnToday btnTime 
         btnDateTime subjectSearch subjectMatches tableSearch tableMatches 
         btnCalcField tableList btnOF tableListOf btnWhere btnMatches 
         subjectBrowse tableBrowse subjectWhereBrowse btnBegins btnAND btnOR 
         btnEQ btnNE btnLT btnGT fieldSearch fieldMatches paramSetSearch 
         btnErrorCheck paramSetMatches btnLE btnGE paramSetBrowse fieldBrowse 
         btnPlus btnMinus subjectParamSetBrowse btnMultiply btnDivide btnMoveUp 
         btnYes btnNo btnDate cUseIndex findType btnDec subjectTableBrowse 
         btnInt columnSearch btnSave columnMatches btnStr subjectColumnBrowse 
         btnSubstr cParameter btnOpen btnSyntax btnClose cConstant btnPeriod 
         btnDouble btnComma btnSingle btnAddSelections queryStr btnGroupCalc 
         btnAddUseIndex btnRemoveUseIndex btnAddParameter btnRemoveSelection 
         btnMoveDown btnAddConstant btnRemove cParameterLabel cConstantLabel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY svRecipients svRunSync svShowAll svShowReportHeader svShowReportFooter 
          svShowPageHeader svShowPageFooter svShowGroupHeader svShowGroupFooter 
          svShowParameters 
      WITH FRAME outputFrame IN WINDOW C-Win.
  ENABLE btnCSV btnLocalCSV svRecipients svRunSync svShowAll svShowReportHeader 
         svShowReportFooter btnPageFormat svShowPageHeader svShowPageFooter 
         svShowGroupHeader svShowGroupFooter svShowParameters btnPrint 
         btnRunResults btnAddEmail btnDOCX btnHTML btnPDF btnView btnXLS 
      WITH FRAME outputFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-outputFrame}
  ENABLE btnCloseResults btnSaveResults 
      WITH FRAME resultsFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-resultsFrame}
  IF AVAILABLE dynSubject THEN 
    DISPLAY dynSubject.isActive dynSubject.isLookup dynSubject.securityLevel 
          dynSubject.subjectTitle dynSubject.subjectType dynSubject.user-id 
          dynSubject.module dynSubject.mnemonic dynSubject.subjectGroup 
          dynSubject.outputFormat dynSubject.externalForm 
          dynSubject.businessLogic dynSubject.subjectAltID 
          dynSubject.useCustList dynSubject.custListID dynSubject.runSync 
          dynSubject.saveLastRun dynSubject.lastRunDateTime 
          dynSubject.recordLimit 
      WITH FRAME viewFrame IN WINDOW C-Win.
  ENABLE btnDelete dynSubject.custListID btnCopy btnPageFormat-2 btnCloseView 
         btnUpdate btnAdd 
      WITH FRAME viewFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-viewFrame}
  ENABLE btnCloseParam 
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
    DEFINE INPUT PARAMETER ipcFieldName AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cDataType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExt      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cField    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTable    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hColumn   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hBuffer   AS HANDLE    NO-UNDO.
    
    DEFINE BUFFER bttSubjectColumn FOR ttSubjectColumn.

    IF CAN-FIND(FIRST ttSubjectColumn
                WHERE ttSubjectColumn.subjectID EQ dynSubject.subjectID
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
    IF NOT lBusinessLogic THEN DO:
        RUN nosweat/get_frmt.p (cTable, cField, OUTPUT cFormat).
        RUN nosweat/fld_lbls.p (cTable, cField, OUTPUT cLabel).
        RUN nosweat/get_type.p (cTable, cField, OUTPUT cDataType).
    END. /* if not business logic */
    ELSE
    IF VALID-HANDLE(hBusinessLogicTable) THEN DO:
        ASSIGN
            hBuffer   = hBusinessLogicTable:DEFAULT-BUFFER-HANDLE
            hColumn   = hBuffer:BUFFER-FIELD(ENTRY(2,ipcFieldName,"."))
            cFormat   = hColumn:FORMAT
            cLabel    = hColumn:LABEL
            cDataType = hColumn:DATA-TYPE
            .
    END. /* else */
    IF cExt NE "" THEN
    cLabel = cLabel + cExt.
    FOR EACH bttSubjectColumn NO-LOCK
        WHERE bttSubjectColumn.subjectID EQ dynSubject.subjectID
           BY bttSubjectColumn.sortOrder DESCENDING
        :
        LEAVE.
    END. /* for each */
    iOrder = IF AVAILABLE bttSubjectColumn THEN bttSubjectColumn.sortOrder ELSE 0.
    CREATE ttSubjectColumn.
    ASSIGN
        ttSubjectColumn.subjectID     = dynSubject.subjectID
        ttSubjectColumn.sortOrder     = iOrder + 1
        ttSubjectColumn.fieldName     = ipcFieldName
        ttSubjectColumn.fieldLabel    = cLabel
        ttSubjectColumn.fieldFormat   = cFormat
        ttSubjectColumn.tableName     = cTable
        ttSubjectColumn.dataType      = cDataType
        ttSubjectColumn.tableDB       = ttField.tableDB
        ttSubjectColumn.isReturnValue = dynSubject.isLookup
        ttSubjectColumn.isSearchable  = dynSubject.isLookup
        ttSubjectColumn.isSortable    = dynSubject.isLookup
        ttSubjectColumn.tableRowID    = ?
        rRowID                        = ROWID(ttSubjectColumn)
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
    
    DEFINE BUFFER bttSubjectParamSet FOR ttSubjectParamSet.
    
    IF CAN-FIND(FIRST ttSubjectParamSet
                WHERE ttSubjectParamSet.subjectID  EQ dynSubject.subjectID
                  AND ttSubjectParamSet.paramSetID EQ ipiParamSetID) THEN
    RETURN.
    FOR EACH bttSubjectParamSet NO-LOCK
        WHERE bttSubjectParamSet.subjectID  EQ dynSubject.subjectID
           BY bttSubjectParamSet.sortOrder DESCENDING
        :
        LEAVE.
    END. /* for each */
    iOrder = IF AVAILABLE bttSubjectParamSet THEN bttSubjectParamSet.sortOrder ELSE 0.
    CREATE ttSubjectParamSet.
    ASSIGN
        ttSubjectParamSet.subjectID  = dynSubject.subjectID
        ttSubjectParamSet.paramSetID = ipiParamSetID
        ttSubjectParamSet.sortOrder  = iOrder + 1
        ttSubjectParamSet.tableRowID = ?
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
    DEFINE VARIABLE jdx AS INTEGER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        CASE subjectSection:
            WHEN "Table" THEN DO:
                DO idx = 1 TO tableBrowse:NUM-SELECTED-ROWS:
                    tableBrowse:FETCH-SELECTED-ROW(idx).
                    RUN pAddTable (ttTable.tableDB, ttTable.TableName, ttTable.tableDscr).
                END. /* do idex */
            END. /* table */
            OTHERWISE DO:
                CASE subjectSection:
                    WHEN "Parameters" THEN
                    jdx = paramSetBrowse:NUM-SELECTED-ROWS.
                    OTHERWISE
                    jdx = fieldBrowse:NUM-SELECTED-ROWS.
                END CASE.
                DO idx = 1 TO jdx:
                    CASE subjectSection:
                        WHEN "Parameters" THEN
                        paramSetBrowse:FETCH-SELECTED-ROW(idx).
                        OTHERWISE
                        fieldBrowse:FETCH-SELECTED-ROW(idx).
                    END CASE.
                    CASE subjectSection:
                        WHEN "Columns" THEN
                        RUN pAddColumn (ttField.fieldName).
                        WHEN "Parameters" THEN
                        RUN pAddParamSet (dynParamSet.paramSetID).
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
                WHERE ttSubjectTable.subjectID EQ dynSubject.subjectID
                  AND ttSubjectTable.tableName EQ ipcTableName) THEN
    RETURN NO-APPLY.
    FOR EACH bttSubjectTable NO-LOCK
        WHERE bttSubjectTable.subjectID EQ dynSubject.subjectID
           BY bttSubjectTable.sortOrder DESCENDING
        :
        LEAVE.
    END. /* for each */
    iOrder = IF AVAILABLE bttSubjectTable THEN bttSubjectTable.sortOrder ELSE 0.
    CREATE ttSubjectTable.
    ASSIGN
        ttSubjectTable.subjectID  = dynSubject.subjectID
        ttSubjectTable.sortOrder  = iOrder + 1
        ttSubjectTable.tableName  = ipcTableName
        ttSubjectTable.tableDscr  = ipcTableDscr
        ttSubjectTable.tableFind  = findType
        ttSubjectTable.tableDB    = ipcTableDB
        ttSubjectTable.tableRowID = ?
        rRowID                    = ROWID(ttSubjectTable)
        .
    {&OPEN-QUERY-subjectTableBrowse}
    REPOSITION subjectTableBrowse TO ROWID rRowID.
    IF ttTable.businessLogic EQ NO THEN
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
            cTable = ENTRY(1,ipcElement,".")
            cField = ENTRY(2,ipcElement,".")        
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
        WHERE bttSubjectWhere.subjectID  EQ dynSubject.subjectID
          AND bttSubjectWhere.whereTable EQ tableList
           BY bttSubjectWhere.sortOrder DESCENDING
        :
        LEAVE.
    END. /* for each */
    iOrder = IF AVAILABLE bttSubjectWhere THEN bttSubjectWhere.sortOrder ELSE 0.
    CREATE ttSubjectWhere.
    ASSIGN
        ttSubjectWhere.subjectID    = dynSubject.subjectID
        ttSubjectWhere.sortOrder    = iOrder + 1
        ttSubjectWhere.tableName    = cTable
        ttSubjectWhere.whereTable   = ipcTableName
        ttSubjectWhere.whereElement = ipcElement
        ttSubjectWhere.fieldLabel   = ipcTableLabel
        ttSubjectWhere.dataType     = cDataType
        ttSubjectWhere.tableRowID   = ?
        rRowID                      = ROWID(ttSubjectWhere)
        .
    {&OPEN-QUERY-subjectWhereBrowse}
    REPOSITION subjectWhereBrowse TO ROWID rRowID.
    fSetSaveButton (YES).
    fShowQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAssign C-Win 
PROCEDURE pAssign :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO TRANSACTION WITH FRAME viewFrame:
        FIND CURRENT dynSubject EXCLUSIVE-LOCK.
        ASSIGN
            {&enabledFields}
            .
        FIND CURRENT dynSubject NO-LOCK.
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCalculatedField C-Win 
PROCEDURE pCalculatedField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFieldFormat AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldLabel  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldList   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCalcFormula AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCalcParam   AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE cCalcProc    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDataType    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParamList   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iSortOrder   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lSave        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE rRowID       AS ROWID     NO-UNDO.
    
    DEFINE BUFFER bttSubjectColumn FOR ttSubjectColumn.
    DEFINE BUFFER bttSubjectWhere  FOR ttSubjectWhere.

    FOR EACH ttField:
        cFieldList = cFieldList + ttField.fieldName + ",".
    END. /* each ttField */
    ASSIGN
        cFieldList = TRIM(cFieldList,",")
        cParamList = cParameter:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME}
        .
    CASE subjectSection:
        WHEN "Columns" THEN DO:
            IF AVAILABLE ttSubjectColumn AND ttSubjectColumn.isCalcField THEN
            ASSIGN
                cFieldName   = ttSubjectColumn.fieldName
                cFieldLabel  = ttSubjectColumn.fieldLabel
                cDataType    = ttSubjectColumn.dataType
                cFieldFormat = ttSubjectColumn.fieldFormat
                cCalcProc    = ttSubjectColumn.calcProc
                cCalcParam   = ttSubjectColumn.calcParam
                cCalcFormula = ttSubjectColumn.calcFormula
                .
            RUN AOA/dynCalcField.w (
                hDynCalcField,
                INPUT-OUTPUT cFieldName,
                INPUT-OUTPUT cFieldLabel,
                INPUT-OUTPUT cDataType,
                INPUT-OUTPUT cFieldFormat,
                INPUT-OUTPUT cCalcProc,
                INPUT-OUTPUT cCalcParam,
                INPUT-OUTPUT cCalcFormula,
                cFieldList,
                ?,
                OUTPUT lSave
                ).
            IF lSave EQ NO THEN RETURN.
            IF NOT AVAILABLE ttSubjectColumn   OR
               NOT ttSubjectColumn.isCalcField THEN DO:
                FOR EACH bttSubjectColumn
                    WHERE bttSubjectColumn.subjectID EQ dynSubject.subjectID
                       BY bttSubjectColumn.sortOrder DESCENDING
                    :
                    iSortOrder = bttSubjectColumn.sortOrder.
                    LEAVE.
                END. /* each bttsubjectcolumn */
                CREATE ttSubjectColumn.
                ASSIGN
                    iSortOrder                  = iSortOrder + 1
                    ttSubjectColumn.subjectID   = dynSubject.subjectID
                    ttSubjectColumn.sortOrder   = iSortOrder
                    ttSubjectColumn.isCalcField = YES
                    .
            END. /* if not avail */
            ASSIGN
                ttSubjectColumn.fieldName   = cFieldName
                ttSubjectColumn.fieldLabel  = cFieldLabel
                ttSubjectColumn.dataType    = cDataType
                ttSubjectColumn.fieldFormat = cFieldFormat
                ttSubjectColumn.calcProc    = cCalcProc
                ttSubjectColumn.calcParam   = cCalcParam
                ttSubjectColumn.calcFormula = cCalcFormula
                rRowID                      = ROWID(ttSubjectColumn)
                .
            {&OPEN-QUERY-subjectColumnBrowse}
            REPOSITION subjectColumnBrowse TO ROWID rRowID.
            fSetSaveButton (YES).
        END. /* columns */
        WHEN "Where" THEN DO:
            IF AVAILABLE ttSubjectWhere AND ttSubjectWhere.isCalcField THEN
            ASSIGN
                cFieldName   = ttSubjectWhere.whereElement
                cFieldName   = REPLACE(cFieldName,"[|","")
                cFieldName   = REPLACE(cFieldName,"|]","")
                cFieldLabel  = ttSubjectWhere.fieldLabel
                cCalcProc    = ttSubjectWhere.calcProc
                cCalcParam   = ttSubjectWhere.calcParam
                cCalcFormula = ""
                cFieldFormat = "x(256)"
                .
            RUN AOA/dynCalcField.w (
                hDynCalcField,
                INPUT-OUTPUT cFieldName,
                INPUT-OUTPUT cFieldLabel,
                INPUT-OUTPUT cDataType,
                INPUT-OUTPUT cFieldFormat,
                INPUT-OUTPUT cCalcProc,
                INPUT-OUTPUT cCalcParam,
                INPUT-OUTPUT cCalcFormula,
                ?,
                cParamList,
                OUTPUT lSave
                ).
            IF lSave EQ NO THEN RETURN.
            IF NOT AVAILABLE ttSubjectWhere   OR
               NOT ttSubjectWhere.isCalcField THEN DO:
                FOR EACH bttSubjectWhere
                    WHERE bttSubjectWhere.subjectID EQ dynSubject.subjectID
                       BY bttSubjectWhere.sortOrder DESCENDING
                    :
                    iSortOrder = bttSubjectWhere.sortOrder.
                    LEAVE.
                END. /* each bttSubjectWhere */
                CREATE ttSubjectWhere.
                ASSIGN
                    iSortOrder                 = iSortOrder + 1
                    ttSubjectWhere.subjectID   = dynSubject.subjectID
                    ttSubjectWhere.sortOrder   = iSortOrder
                    ttSubjectWhere.whereTable  = tableList
                    ttSubjectWhere.isCalcField = YES
                    .
            END. /* if not avail */
            ASSIGN
                cFieldName = "[|" + cFieldName + "|]"
                ttSubjectWhere.whereElement = cFieldName
                ttSubjectWhere.fieldLabel   = cFieldLabel
                ttSubjectWhere.calcProc     = cCalcProc
                ttSubjectWhere.calcParam    = cCalcParam
                rRowID                      = ROWID(ttSubjectWhere)
                .
            {&OPEN-QUERY-subjectWhereBrowse}
            REPOSITION subjectWhereBrowse TO ROWID rRowID.
            fSetSaveButton (YES).
            fShowQuery().
        END. /* parameters */
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCallAudit C-Win 
PROCEDURE pCallAudit :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hTable AS HANDLE    NO-UNDO.

    CASE subjectSection:
        WHEN "Columns" THEN DO:
            FIND FIRST dynSubjectColumn NO-LOCK
                 WHERE ROWID(dynSubjectColumn) EQ ttSubjectColumn.tableRowID
                 NO-ERROR.
            IF NOT AVAILABLE dynSubjectColumn THEN RETURN.
            hTable = BUFFER dynSubjectColumn:HANDLE.
        END.
        WHEN "Parameters" THEN DO:
            FIND FIRST dynSubjectParamSet NO-LOCK
                 WHERE ROWID(dynSubjectParamSet) EQ ttSubjectParamSet.tableRowID
                 NO-ERROR.
            IF NOT AVAILABLE dynSubjectParamSet THEN RETURN.
            hTable = BUFFER dynSubjectParamSet:HANDLE.
        END.
        WHEN "Subject" THEN DO:
            IF NOT AVAILABLE dynSubject THEN RETURN.
            hTable = BUFFER dynSubject:HANDLE.
        END.
        WHEN "Table" THEN DO:
            FIND FIRST dynSubjectTable NO-LOCK
                 WHERE ROWID(dynSubjectTable) EQ ttSubjectTable.tableRowID
                 NO-ERROR.
            IF NOT AVAILABLE dynSubjectTable THEN RETURN.
            hTable = BUFFER dynSubjectTable:HANDLE.
        END.
        WHEN "Where" THEN DO:
            FIND FIRST dynSubjectWhere NO-LOCK
                 WHERE ROWID(dynSubjectWhere) EQ ttSubjectWhere.tableRowID
                 NO-ERROR.
            IF NOT AVAILABLE dynSubjectWhere THEN RETURN.
            hTable = BUFFER dynSubjectWhere:HANDLE.
        END.
    END CASE.
    IF VALID-HANDLE(hTable) THEN
    RUN system/CallAudit.p (hTable:NAME, hTable, "Window", PROGRAM-NAME(1)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pClearView C-Win 
PROCEDURE pClearView :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
    
    DO WITH FRAME viewFrame:
        ASSIGN
            hWidget = FRAME viewFrame:HANDLE
            hWidget = hWidget:FIRST-CHILD
            hWidget = hWidget:FIRST-CHILD
            .
        DO WHILE VALID-HANDLE(hWidget):
            IF hWidget:TYPE NE "BUTTON" AND
               hWidget:SELECTABLE EQ NO AND 
               hWidget:SENSITIVE THEN
            hWidget:SCREEN-VALUE = IF hWidget:TYPE EQ "TOGGLE-BOX" THEN "NO" ELSE "".
            hWidget = hWidget:NEXT-SIBLING.
        END. /* do while */
    END. /* with frame */

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

    DEFINE VARIABLE cMsg           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iNextSubjectID AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lContinue      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lCopyColumn    AS LOGICAL   NO-UNDO INITIAL YES.
    DEFINE VARIABLE lCopyParamSet  AS LOGICAL   NO-UNDO INITIAL YES.
    DEFINE VARIABLE lCopyTable     AS LOGICAL   NO-UNDO INITIAL YES.
    DEFINE VARIABLE lCopyWhere     AS LOGICAL   NO-UNDO INITIAL YES.
    DEFINE VARIABLE rRowID         AS ROWID     NO-UNDO.
    
    DEFINE BUFFER bDynSubjectColumn   FOR dynSubjectColumn.
    DEFINE BUFFER bDynSubjectParamSet FOR dynSubjectParamSet.
    DEFINE BUFFER bDynSubjectTable    FOR dynSubjectTable.
    DEFINE BUFFER bDynSubjectWhere    FOR dynSubjectWhere.

    DO WITH FRAME viewFrame:
        CASE iphMode:LABEL:
            WHEN "Add" OR WHEN "Copy" OR WHEN "Update" THEN DO:
                DISABLE {&transPanel}.
                ENABLE {&transUpdate} {&enabledFields}.
                DISABLE dynSubject.lastRunDateTime.
                btnUpdate:LOAD-IMAGE("Graphics\32x32\floppy_disk.png").
                ASSIGN
                    dynSubject.subjectType:SENSITIVE = lSuperAdmin
                    iCopySubjectID = ?
                    .
                IF AVAILABLE dynSubject AND iphMode:LABEL EQ "Copy" THEN
                iCopySubjectID = dynSubject.subjectID.
                CASE iphMode:LABEL:
                    WHEN "Add" THEN DO:
                        RUN pClearView.
                        ASSIGN
                            dynSubject.subjectID:SCREEN-VALUE     = ""
                            dynSubject.isActive:SCREEN-VALUE      = "yes"
                            dynSubject.securityLevel:SCREEN-VALUE = STRING(iUserSecurityLevel)
                            dynSubject.user-id:SCREEN-VALUE       = "{&defaultUser}"
                            dynSubject.subjectType:SCREEN-VALUE   = IF lSuperAdmin THEN "System" ELSE "User"
                            dynSubject.outputFormat:SCREEN-VALUE  = "Grid"
                            .
                        DISABLE btnReset.
                    END. /* add */
                    WHEN "Copy" THEN
                    ASSIGN
                        dynSubject.subjectID:SCREEN-VALUE   = ""
                        dynSubject.subjectType:SCREEN-VALUE = IF lSuperAdmin THEN "System" ELSE "User"
                        .
                END CASE.
                ASSIGN
                    FRAME viewFrame:TITLE = iphMode:LABEL
                    btnUpdate:LABEL = "Save"
                    .
            END. /* add copy update */
            WHEN "Cancel" OR WHEN "Save" THEN DO:
                IF iphMode:LABEL EQ "Save" THEN DO:
                    IF cMode EQ "Add" OR cMode EQ "Copy" THEN DO TRANSACTION:
                        DO WHILE TRUE:
                            iNextSubjectID = NEXT-VALUE(subjectID).
                            IF dynSubject.subjectType:SCREEN-VALUE NE "System" THEN
                            iNextSubjectID = iNextSubjectID + 5000.
                            IF NOT CAN-FIND(FIRST dynSubject
                                WHERE dynSubject.subjectID EQ iNextSubjectID) THEN
                            LEAVE.
                        END. /* do while */
                        CREATE dynSubject.
                        ASSIGN
                            dynSubject.subjectID:SCREEN-VALUE = STRING(iNextSubjectID)
                            dynSubject.subjectID = iNextSubjectID
                            rRowID = ROWID(dynSubject)
                            .
                    END. /* if add/copy */
                    RUN pAssign.
                    IF cMode EQ "Add" OR cMode EQ "Copy" THEN DO:
                        RUN pReopenBrowse.
                        REPOSITION subjectBrowse TO ROWID rRowID.
                        IF cMode EQ "Copy" THEN DO:
                            RUN AOA/dynSubjctCopy.w (
                                OUTPUT lCopyTable,
                                OUTPUT lCopyWhere,
                                OUTPUT lCopyColumn,
                                OUTPUT lCopyParamSet
                                ).
                            IF lCopyTable THEN
                            {AOA/includes/dynSubjctCopy.i "SubjectTable"}
                            IF lCopyWhere THEN
                            {AOA/includes/dynSubjctCopy.i "SubjectWhere"}
                            IF lCopyColumn THEN
                            {AOA/includes/dynSubjctCopy.i "SubjectColumn"}
                            IF lCopyParamSet THEN
                            {AOA/includes/dynSubjctCopy.i "SubjectParamSet"}
                        END. /* if copy */
                    END. /* if add/copy */
                    ELSE
                    BROWSE subjectBrowse:REFRESH().
                END. /* save */
                DISABLE {&transPanel} {&enabledFields}.
                ENABLE {&transInit}.
                btnUpdate:LOAD-IMAGE("Graphics\32x32\Pencil.png").
                ASSIGN
                    FRAME viewFrame:TITLE = "View"
                    btnUpdate:LABEL = "Update"
                    .
                APPLY "VALUE-CHANGED":U TO BROWSE subjectBrowse.
            END. /* cancel save */
            WHEN "Delete" THEN DO:
                IF AVAILABLE dynSubject THEN DO:
                    IF CAN-FIND(FIRST dynParamValue
                        WHERE dynParamValue.subjectID EQ dynSubject.subjectID
                          AND dynParamValue.user-id NE "{&defaultUser}") OR
                       CAN-FIND(FIRST Task
                        WHERE Task.subjectID EQ dynSubject.subjectID
                          AND Task.user-id NE "{&defaultUser}") THEN
                    cMsg = "Parameter Value and/or User Tasks will also be Deleted!" + CHR(10) + CHR(10).
                    MESSAGE
                        cMsg
                        "Delete Currently Selected Record?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    UPDATE lContinue.
                    SESSION:SET-WAIT-STATE ("General").
                    IF lContinue THEN DO TRANSACTION:
                        cMode = iphMode:LABEL.
                        {AOA/includes/dynSubjctDelete.i "Task"}
                        {AOA/includes/dynSubjctDelete.i "dynParamValue"}
                        {AOA/includes/dynSubjctDelete.i "dynSubjectTable"}
                        {AOA/includes/dynSubjctDelete.i "dynSubjectWhere"}
                        {AOA/includes/dynSubjctDelete.i "dynSubjectColumn"}
                        {AOA/includes/dynSubjctDelete.i "dynSubjectParamSet"}
                        FIND CURRENT dynSubject EXCLUSIVE-LOCK.
                        DELETE dynSubject.
                        BROWSE subjectBrowse:DELETE-CURRENT-ROW().
                    END. /* if lcontinue */
                    IF AVAILABLE dynSubject THEN
                    BROWSE subjectBrowse:REFRESH().
                    RUN pDisplay.
                    SESSION:SET-WAIT-STATE ("").
                END. /* if avail */
            END. /* delete */
            WHEN "Reset" THEN DO:
                RUN pDisplay.
                DISABLE {&transPanel}.
                ENABLE {&transUpdate}.
            END. /* reset */
        END CASE. /* ipcmode:label */
        IF dynSubject.isActive:SENSITIVE THEN
        APPLY "ENTRY":U TO dynSubject.isActive.
        ELSE
        APPLY "ENTRY":U TO BROWSE subjectBrowse.
        /* save the mode for when logic returns to this procedure */
        cMode = iphMode:LABEL.
    END. /* do frame */    
    fSetSaveButton (NO).
    fShowQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDeleteProcedure C-Win 
PROCEDURE pDeleteProcedure :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   IF VALID-HANDLE(hAppSrvBin) THEN
   DELETE PROCEDURE hAppSrvBin.
   IF VALID-HANDLE(hJasper) THEN
   DELETE PROCEDURE hJasper.
   IF VALID-HANDLE(hDynCalcField) THEN
   DELETE PROCEDURE hDynCalcField.
   IF VALID-HANDLE(hDynDescripProc) THEN
   DELETE PROCEDURE hDynDescripProc.
   IF VALID-HANDLE(hDynInitProc) THEN
   DELETE PROCEDURE hDynInitProc.
   IF VALID-HANDLE(hDynValProc) THEN
   DELETE PROCEDURE hDynValProc.

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
            WHERE ttSubjectColumn.subjectID   EQ dynSubject.subjectID
              AND ttSubjectColumn.tableName EQ ttSubjectTable.tableName) THEN DO:
        FOR EACH ttSubjectColumn
            WHERE ttSubjectColumn.subjectID   EQ dynSubject.subjectID
              AND ttSubjectColumn.tableName EQ ttSubjectTable.tableName
            :
            DELETE ttSubjectColumn.
        END. /* each ttSubjectColumn*/
        subjectSection = "Column".
        RUN pSetOrder.
    END. /* if can-find */
    IF CAN-FIND(FIRST ttSubjectWhere
            WHERE ttSubjectWhere.subjectID   EQ dynSubject.subjectID
              AND ttSubjectWhere.tableName EQ ttSubjectTable.tableName) THEN DO:
        FOR EACH ttSubjectWhere
            WHERE ttSubjectWhere.subjectID   EQ dynSubject.subjectID
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplay C-Win 
PROCEDURE pDisplay :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME viewFrame:
        IF AVAILABLE dynSubject THEN DO:
            ASSIGN
                dynSubject.custListID:SCREEN-VALUE   = " "
                dynSubject.module:SCREEN-VALUE       = " "
                dynSubject.subjectGroup:SCREEN-VALUE = " "
                .
            DISPLAY {&displayFields}.
            ENABLE {&transInit}.
        END. /* if avail */
        ELSE DO:
            RUN pClearView.
            DISABLE {&transPanel}.
            ENABLE btnAdd btnCloseView.
        END. /* else */
    END. /* with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pErrorCheck C-Win 
PROCEDURE pErrorCheck :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    DEFINE VARIABLE lErrors AS LOGICAL NO-UNDO.
    
    RUN AOA/dynSubjectCheck.p (OUTPUT lErrors).

    OUTPUT TO c:\tmp\DynSubjectErrors.txt APPEND.
    PUT UNFORMATTED
        "*** Dynamic Subject Sanity Check Report ***" SKIP
        "-------------------------------------------" SKIP
        .
    FOR EACH dynsubject NO-LOCK:
        DISPLAY
            dynsubject.subjectid
            dynsubject.subjecttitle
                WITH STREAM-IO.
        FOR EACH dynsubjecttable OF dynsubject NO-LOCK
            BY dynsubjecttable.sortorder
            :
            DISPLAY
                dynsubjecttable.sortorder
                dynsubjecttable.tablename
                    WITH STREAM-IO.
            idx = 0.
            FOR EACH dynsubjectwhere OF dynsubject NO-LOCK
                WHERE dynsubjectwhere.wheretable EQ dynsubjecttable.tablename
                   BY dynsubjectwhere.subjectid
                   BY dynsubjectwhere.wheretable
                   BY dynsubjectwhere.sortorder
                :
                idx = idx + 1.
                DISPLAY
                    dynsubjectwhere.subjectid
                    dynsubjectwhere.sortorder
                    dynsubjectwhere.whereelement
                    "error" WHEN idx NE dynsubjectwhere.sortorder
                        WITH STREAM-IO WIDTH 200.
                IF lErrors EQ NO THEN
                lErrors = idx NE dynsubjectwhere.sortorder.
                idx = dynsubjectwhere.sortorder.
            END.
        END.
    END.
    OUTPUT CLOSE.
    MESSAGE 
        CAPS(STRING(lErrors)) "Errors Found, View Report?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    UPDATE lErrors.
    IF lErrors THEN
    OS-COMMAND NO-WAIT notepad.exe c:\tmp\DynSubjectErrors.txt.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGenerateDefsInclude C-Win 
PROCEDURE pGenerateDefsInclude :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cInclude    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPriorParam AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVariable   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx         AS INTEGER   NO-UNDO.
    
    /* ensure needed folders exist */
    OS-CREATE-DIR "AOA".
    OS-CREATE-DIR "AOA/includes".

    cInclude = "AOA/includes/subjectID"
             + STRING(dynParamValue.subjectID)
             + "Defs.i"
             .
    OUTPUT TO VALUE(cInclude).
    PUT UNFORMATTED
        "/* subjectID" STRING(dynParamValue.subjectID) + "Defs.i - "
        "auto generated "
        STRING(TODAY,"99.99.9999") " @ "
        STRING(TIME,"hh:mm:ss am") " */"
        SKIP(1)
        "~{AOA/includes/dynRunBusinessLogicDefs.i}" SKIP(1)
        "/* parameter values loaded into these variables */"
        SKIP
        .
    FOR EACH dynValueParam NO-LOCK
        WHERE dynValueParam.subjectID    EQ dynParamValue.subjectID
          AND dynValueParam.user-id      EQ dynParamValue.user-id
          AND dynValueParam.prgmName     EQ dynParamValue.prgmName
          AND dynValueParam.paramValueID EQ dynParamValue.paramValueID
           BY dynValueParam.sortOrder
        :
        cVariable = fGetVariable(dynValueParam.paramName,dynValueParam.dataType).
        PUT UNFORMATTED
            "DEFINE VARIABLE " cVariable            
            " AS " CAPS(dynValueParam.dataType) " NO-UNDO."
            SKIP.
    END. /* each dynvalueparam */
    PUT UNFORMATTED
        SKIP(1)
        "PROCEDURE pAssignParamVariables:" SKIP
        "    /* load dynamic parameter values into variables */"
        SKIP
        "    ASSIGN"
        SKIP.
    FOR EACH dynValueParam NO-LOCK
        WHERE dynValueParam.subjectID    EQ dynParamValue.subjectID
          AND dynValueParam.user-id      EQ dynParamValue.user-id
          AND dynValueParam.prgmName     EQ dynParamValue.prgmName
          AND dynValueParam.paramValueID EQ dynParamValue.paramValueID
           BY dynValueParam.sortOrder
        :
        cVariable = fGetVariable(dynValueParam.paramName,dynValueParam.dataType).
        PUT UNFORMATTED FILL(" ",8) cVariable " = ".
        IF dynValueParam.dataType EQ "Date" THEN
        PUT UNFORMATTED "DATE(".
        PUT UNFORMATTED
            "DYNAMIC-FUNCTION(~"fGetDynParamValue~",~""
            dynValueParam.paramName "~")"
            .
        IF dynValueParam.dataType EQ "Date" THEN
        PUT UNFORMATTED ")".
        IF dynValueParam.dataType EQ "Logical" THEN
        PUT UNFORMATTED " EQ ~"YES~"".
        PUT UNFORMATTED SKIP.
        IF dynValueParam.dataType EQ "Date" THEN
        cPriorParam = cVariable.
        IF cPriorParam NE "" AND cPriorParam NE cVariable THEN DO:
            PUT UNFORMATTED FILL(" ",8)
                cPriorParam " = DYNAMIC-FUNCTION(~"fDateOptionDate~","
                cVariable "," cPriorParam ")"
                SKIP.
            cPriorParam = "".
        END. /* if cpriorparam */
    END. /* each dynvalueparam */
    PUT UNFORMATTED FILL(" ",8) "." SKIP
        "END PROCEDURE." SKIP.
    OUTPUT CLOSE.
    MESSAGE
        "Subject ID:" dynParamValue.subjectID SKIP
        "Include generated with start in folder as" SKIP
        cInclude SKIP(1)
        "View Generated Include?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    UPDATE lViewInclude AS LOGICAL.
    IF lViewInclude THEN
    OS-COMMAND NO-WAIT notepad.exe VALUE(cInclude).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetBusinessLogicTable C-Win 
PROCEDURE pGetBusinessLogicTable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER cBusinessLogic AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cBLFile AS CHARACTER NO-UNDO.
    
    ASSIGN
        cBLFile = REPLACE(cBusinessLogic,".p",".r")
        cBLFile = SEARCH(cBLFile)
        .
    IF cBLFile EQ ? THEN
    ASSIGN
        cBLFile = REPLACE(cBusinessLogic,".r",".p")
        cBLFile = SEARCH(cBLFile)
        .
    IF cBLFile NE ? THEN DO:
        RUN VALUE(cBLFile) PERSISTENT SET hBusinessLogic.
        hBusinessLogicTable = DYNAMIC-FUNCTION("fGetTableHandle" IN hBusinessLogic).
        IF CAN-FIND(FIRST ttTable
                    WHERE ttTable.tableName EQ hBusinessLogicTable:NAME
                      AND ttTable.businessLogic EQ YES) THEN
        RETURN.
        CREATE ttTable.
        ASSIGN
            ttTable.tableName     = hBusinessLogicTable:NAME
            ttTable.tableDB       = "ASI"
            ttTable.businessLogic = YES
            .
        RUN pDeleteProcedure IN hBusinessLogic.
    END. /* if search */
    ELSE DO:
        MESSAGE
            "Business Logic" cBusinessLogic "does not Exist!"
        VIEW-AS ALERT-BOX ERROR.
    END. /* else */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetCustListID C-Win 
PROCEDURE pGetCustListID :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.

    RUN spGetSessionParam ("Company", OUTPUT cCompany).
    FOR EACH sys-ctrl-shipto NO-LOCK
        WHERE sys-ctrl-shipto.company EQ cCompany
          AND sys-ctrl-shipto.name    EQ "CustomerList"
        BREAK BY sys-ctrl-shipto.char-fld
        :
        IF FIRST-OF(sys-ctrl-shipto.char-fld) THEN
        dynSubject.custListID:ADD-LAST(sys-ctrl-shipto.char-fld) IN FRAME viewFrame.
    END. /* each sys-ctrl-shipto */

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
        ASSIGN
            cTableName = cTableName + ",_file,_field"
            cTableDscr = cTableDscr + ",VST File,VST Field"
            .
        IF cTableDscr EQ "" OR cTableDscr EQ ? THEN
        cTableDscr = cTableName.
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
    DEFINE VARIABLE hBuffer    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hColumn    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.

    DEFINE BUFFER ttSubjectTable FOR ttSubjectTable.
    
    EMPTY TEMP-TABLE ttField.
    ASSIGN
        tableList:LIST-ITEMS IN FRAME {&FRAME-NAME} = ?
        tableListOf:LIST-ITEMS = ?
        .
    IF NOT lBusinessLogic THEN
    FOR EACH ttSubjectTable
        WHERE ttSubjectTable.subjectID EQ dynSubject.subjectID
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
            IF cLabel EQ "" OR cLabel EQ ? THEN
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
    ELSE
    IF VALID-HANDLE(hBusinessLogicTable) THEN DO:
        hBuffer = hBusinessLogicTable:DEFAULT-BUFFER-HANDLE.
        DO idx = 1 TO hBuffer:NUM-FIELDS:
            hColumn = hBuffer:BUFFER-FIELD(idx).
            IF hColumn:NAME BEGINS "xx" THEN NEXT.
            CREATE ttField.
            ASSIGN
                ttField.tableDB    = "ASI"
                ttField.fieldLabel = hColumn:LABEL
                ttField.fieldName  = hBusinessLogicTable:NAME + "."
                                   + hColumn:NAME
                                   .
        END. /* else, business logic */
    END. /* if valid-handle */
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
        WHERE ttSubjectParamSet.subjectID EQ dynSubject.subjectID,
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
        IF LOOKUP(cParamName,cParameter:LIST-ITEM-PAIRS) EQ 0 OR
           cParameter:LIST-ITEM-PAIRS EQ ? THEN 
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
         WHERE user-print.program-id EQ cPrgmName
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pLoadSubject C-Win 
PROCEDURE pLoadSubject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    EMPTY TEMP-TABLE ttSubjectTable.
    EMPTY TEMP-TABLE ttSubjectWhere.
    EMPTY TEMP-TABLE ttSubjectColumn.
    EMPTY TEMP-TABLE ttSubjectParamSet.
    EMPTY TEMP-TABLE ttGroupCalc.
    
    dynSubject.subjectGroup:LIST-ITEMS IN FRAME viewFrame = ",".
    FOR EACH usergrps NO-LOCK
        :
        dynSubject.subjectGroup:ADD-LAST(usergrps.usergrps).
    END. /* for each */

    FOR EACH dynSubjectTable NO-LOCK
        WHERE dynSubjectTable.subjectID {&GT} 0
        :
        CREATE ttSubjectTable.
        BUFFER-COPY dynSubjectTable TO ttSubjectTable
            ASSIGN ttSubjectTable.tableRowID = ROWID(dynSubjectTable).
    END. /* for each */

    FOR EACH dynSubjectWhere NO-LOCK
        WHERE dynSubjectWhere.subjectID {&GT} 0
        :
        CREATE ttSubjectWhere.
        BUFFER-COPY dynSubjectWhere TO ttSubjectWhere
            ASSIGN ttSubjectWhere.tableRowID = ROWID(dynSubjectWhere).
    END. /* for each */

    FOR EACH dynSubjectColumn NO-LOCK
        WHERE dynSubjectColumn.subjectID {&GT} 0
        :
        CREATE ttSubjectColumn.
        BUFFER-COPY dynSubjectColumn TO ttSubjectColumn
            ASSIGN ttSubjectColumn.tableRowID = ROWID(dynSubjectColumn).
        IF ttSubjectColumn.groupCalc NE "" THEN
        DO idx = 1 TO NUM-ENTRIES(ttSubjectColumn.groupCalc) BY 2:
            CREATE ttGroupCalc.
            ASSIGN
                ttGroupCalc.subjectID = ttSubjectColumn.subjectID
                ttGroupCalc.fieldName = ttSubjectColumn.fieldName
                ttGroupCalc.groupName = ENTRY(idx,ttSubjectColumn.groupCalc)
                ttGroupCalc.calcType  = ENTRY(idx + 1,ttSubjectColumn.groupCalc)
                .
        END. /* do idx */
    END. /* for each */

    FOR EACH dynSubjectParamSet NO-LOCK
        WHERE dynSubjectParamSet.subjectID {&GT} 0
        :
        CREATE ttSubjectParamSet.
        BUFFER-COPY dynSubjectParamSet TO ttSubjectParamSet
            ASSIGN ttSubjectParamSet.tableRowID = ROWID(dynSubjectParamSet).
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
    
    DEFINE VARIABLE iCurrent   AS INTEGER NO-UNDO.
    DEFINE VARIABLE iMoveTo    AS INTEGER NO-UNDO.
    DEFINE VARIABLE iSubjectID AS INTEGER NO-UNDO.
    DEFINE VARIABLE rRowID     AS ROWID   NO-UNDO.
    
    DEFINE BUFFER bttSubjectColumn   FOR ttSubjectColumn.
    DEFINE BUFFER bttSubjectParamSet FOR ttSubjectParamSet.
    DEFINE BUFFER bttSubjectTable    FOR ttSubjectTable.
    DEFINE BUFFER bttSubjectWhere    FOR ttSubjectWhere.
    
    iSubjectID = dynSubject.subjectID.
    CASE subjectSection:
        WHEN "Columns" THEN DO:
            {AOA/includes/pMove.i "ttSubjectColumn" "subjectColumnBrowse"}
        END. /* columns */
        WHEN "Parameters" THEN DO:
            {AOA/includes/pMove.i "ttSubjectParamSet" "subjectParamSetBrowse"}
        END. /* table */
        WHEN "Table" THEN DO:
            {AOA/includes/pMove.i "ttSubjectTable" "subjectTableBrowse"}
        END. /* table */
        WHEN "Where" THEN DO:
            {AOA/includes/pMove.i "ttSubjectWhere" "subjectWhereBrowse" tableList}
        END. /* where */
    END CASE.
    fSetSaveButton (YES).
    fShowQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPageFormat C-Win 
PROCEDURE pPageFormat :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cPageOrientation AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iPageFormat      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPageHeight      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPageWidth       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lContinue        AS LOGICAL   NO-UNDO.

    ASSIGN
        iPageFormat      = dynSubject.pageFormat
        cPageOrientation = dynSubject.pageOrientation
        iPageWidth       = dynSubject.pageWidth
        iPageHeight      = dynSubject.pageHeight
        .
    RUN AOA/dynPageFormat.w (
        INPUT-OUTPUT iPageFormat,
        INPUT-OUTPUT cPageOrientation,
        INPUT-OUTPUT iPageWidth,
        INPUT-OUTPUT iPageHeight,
        OUTPUT lContinue
        ).
    IF lContinue THEN
    DO TRANSACTION:
        FIND CURRENT dynSubject EXCLUSIVE-LOCK.
        ASSIGN
            dynSubject.pageFormat      = iPageFormat
            dynSubject.pageOrientation = cPageOrientation
            dynSubject.pageWidth       = iPageWidth
            dynSubject.pageHeight      = iPageHeight
            .
        FIND CURRENT dynSubject NO-LOCK.
    END. /* do trans */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQueryDesign C-Win 
PROCEDURE pQueryDesign :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lSave AS LOGICAL NO-UNDO.

    RUN AOA/qryDesign.w (
        dynSubject.subjectID,
        THIS-PROCEDURE:HANDLE,
        hDynCalcField,
        OUTPUT lSave
        ).
    IF lSave THEN DO TRANSACTION:
        FOR EACH ttSubjectTable
            WHERE ttSubjectTable.subjectID EQ dynSubject.subjectID
            :
            DELETE ttSubjectTable.
        END. /* each ttSubjectTable */
        FOR EACH ttSubjectWhere
            WHERE ttSubjectWhere.subjectID EQ dynSubject.subjectID
            :
            DELETE ttSubjectWhere.
        END. /* each ttSubjectWhere */
        FOR EACH dynSubjectTable NO-LOCK
            WHERE dynSubjectTable.subjectID EQ dynSubject.subjectID
            :
            CREATE ttSubjectTable.
            BUFFER-COPY dynSubjectTable TO ttSubjectTable
                ASSIGN ttSubjectTable.tableRowID = ROWID(dynSubjectTable).
        END. /* for each */
    
        FOR EACH dynSubjectWhere NO-LOCK
            WHERE dynSubjectWhere.subjectID EQ dynSubject.subjectID
            :
            CREATE ttSubjectWhere.
            BUFFER-COPY dynSubjectWhere TO ttSubjectWhere
                ASSIGN ttSubjectWhere.tableRowID = ROWID(dynSubjectWhere).
        END. /* for each */
        RUN pSaveSubject.
        APPLY "VALUE-CHANGED":U TO BROWSE subjectBrowse.
    END. /* if save */

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
            WHEN "Parameters" THEN DO:
                DO idx = 1 TO subjectParamSetBrowse:NUM-SELECTED-ROWS:
                    subjectParamSetBrowse:FETCH-SELECTED-ROW(idx).
                    DELETE ttSubjectParamSet.
                END. /* do idex */
            END. /* parameters */
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
        WHEN "module" THEN
        RUN pByModule.
        WHEN "paramSetID" THEN
        RUN pByParamSetID.
        WHEN "setName" THEN
        RUN pBySetName.
        WHEN "subjectID" THEN
        RUN pBySubjectID.
        WHEN "subjectTitle" THEN
        RUN pBySubjectTitle.
        WHEN "subjectType" THEN
        RUN pBySubjectType.
        WHEN "tableDB" THEN
        RUN pByTableDB.
        WHEN "tableDscr" THEN
        RUN pByTableDscr.
        WHEN "tableName" THEN
        RUN pByTableName.
        OTHERWISE
        {&OPEN-QUERY-subjectBrowse}
    END CASE.
    {AOA/includes/pReopenBrowse.i}
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
        IF cDBName EQ "PROGRESST" THEN
        cDBName = "ASI".
        IF hQueryBrowse:GET-BROWSE-COLUMN(idx):INDEX NE 0 THEN
        cFieldName = cFieldName
                   + "["
                   + STRING(hQueryBrowse:GET-BROWSE-COLUMN(idx):INDEX)
                   + "]"
                   .
        FIND FIRST ttSubjectColumn
             WHERE ttSubjectColumn.subjectID EQ dynSubject.subjectID
               AND ttSubjectColumn.tableDB   EQ cDBName
               AND ttSubjectColumn.tableName EQ cTableName
               AND ttSubjectColumn.fieldName EQ cFieldName
             NO-ERROR.
        IF NOT AVAILABLE ttSubjectColumn THEN NEXT.
        IF ttSubjectColumn.sortOrder  NE idx OR
           ttSubjectColumn.columnSize NE dSize THEN DO:
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
         WHERE user-print.program-id EQ cPrgmName
           AND user-print.user-id    EQ USERID("ASI")
         NO-ERROR.
    IF NOT AVAILABLE user-print THEN DO:
        CREATE user-print.
        ASSIGN
            user-print.program-id = cPrgmName
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
    FIND CURRENT user-print NO-LOCK.

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
    DEFINE BUFFER bttSubjectTable    FOR ttSubjectTable.
    DEFINE BUFFER bttSubjectWhere    FOR ttSubjectWhere.
    DEFINE BUFFER bttSubjectColumn   FOR ttSubjectColumn.
    DEFINE BUFFER bttSubjectParamSet FOR ttSubjectParamSet.
    
    DO TRANSACTION:
        {AOA/includes/dynSubjctSave.i "Table"}
        {AOA/includes/dynSubjctSave.i "Where"}
        {AOA/includes/dynSubjctSave.i "Column"}
        {AOA/includes/dynSubjctSave.i "ParamSet"}
        FIND CURRENT dynSubject EXCLUSIVE-LOCK.
        dynSubject.queryStr = queryStr.
        FIND CURRENT dynSubject NO-LOCK.
    END. /* do trans */
    IF subjectSection EQ "Parameters" AND
       VALID-HANDLE(hParamBldr) THEN
    RUN pReset IN hParamBldr.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSection C-Win 
PROCEDURE pSection :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphSection AS HANDLE    NO-UNDO.
    
    DEFINE VARIABLE i AS INTEGER NO-UNDO.

    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).

    DO WITH FRAME {&FRAME-NAME}:
        IF FRAME viewFrame:HIDDEN EQ NO THEN RETURN.
        HIDE {&allSection}.
        ASSIGN
            subjectSection = iphSection:PRIVATE-DATA
            btnSave:HIDDEN = NOT lSave
            btnUndo:HIDDEN = btnSave:HIDDEN
            columnsSectionLabel:BGCOLOR   = 22
            columnsSectionLabel:FGCOLOR   = 0
            paramSetsSectionLabel:BGCOLOR = 22
            paramSetsSectionLabel:FGCOLOR = 0
            subjectSectionLabel:BGCOLOR   = 22
            subjectSectionLabel:FGCOLOR   = 0
            tableSectionLabel:BGCOLOR     = 22
            tableSectionLabel:FGCOLOR     = 0
            whereSectionLabel:BGCOLOR     = 22
            whereSectionLabel:FGCOLOR     = 0
            .
        hSection:LOAD-IMAGE("Graphics/32x32/tabdown72.png").
        iphSection:LOAD-IMAGE("Graphics/32x32/tabup72.png").
        hSection = iphSection.
        CASE subjectSection:
            WHEN "Columns" THEN DO:
                ASSIGN
                    columnsSectionLabel:BGCOLOR = 29
                    columnsSectionLabel:FGCOLOR = 15
                    .
                VIEW {&columnsSection}.
            END.
            WHEN "Parameters" THEN DO:
                ASSIGN
                    paramSetsSectionLabel:BGCOLOR = 29
                    paramSetsSectionLabel:FGCOLOR = 15
                    .
                VIEW {&parameterSection}.
            END.
            WHEN "Subject" THEN DO:
                VIEW {&subjectSection}.
                ASSIGN
                    subjectSectionLabel:BGCOLOR = 29
                    subjectSectionLabel:FGCOLOR = 15
                    .
            END.
            WHEN "Table" THEN DO:
                VIEW {&tableSection}.
                ASSIGN
                    tableSectionLabel:BGCOLOR = 29
                    tableSectionLabel:FGCOLOR = 15
                    .
            END.
            WHEN "Where" THEN DO:
                VIEW {&whereSection}.
                ASSIGN
                    whereSectionLabel:BGCOLOR = 29
                    whereSectionLabel:FGCOLOR = 15
                    .
            END.
        END CASE.
        BROWSE subjectBrowse:WIDTH = IF subjectSection NE "Subject" THEN 37
            ELSE FRAME {&FRAME-NAME}:WIDTH - BROWSE subjectBrowse:COL.
    END. /* do with frame */
    RUN LockWindowUpdate (0,OUTPUT i).

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
        WHERE ttGroupCalc.subjectID EQ dynSubject.subjectID
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
            subjectParamSetBrowse:COL = {&COL}
            subjectParamSetBrowse:ROW = {&ROW} + 1.67            
            
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
    
    DEFINE BUFFER bttSubjectColumn   FOR ttSubjectColumn.
    DEFINE BUFFER bttSubjectParamSet FOR ttSubjectParamSet.
    DEFINE BUFFER bttSubjectTable    FOR ttSubjectTable.
    DEFINE BUFFER bttSubjectWhere    FOR ttSubjectWhere.
    
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN tableList.
    END. /* do with */
    CASE subjectSection:
        WHEN "Columns" THEN DO:
            {AOA/includes/pSetOrder.i "ttSubjectColumn" "subjectColumnBrowse"}
        END. /* columns */
        WHEN "Parameters" THEN DO:
            {AOA/includes/pSetOrder.i "ttSubjectParamSet" "subjectParamSetBrowse"}
        END. /* where */
        WHEN "Table" THEN DO:
            {AOA/includes/pSetOrder.i "ttSubjectTable" "subjectTableBrowse"}
        END. /* table */
        WHEN "Where" THEN DO:
            {AOA/includes/pSetOrder.i "ttSubjectWhere" "subjectWhereBrowse"}
        END. /* where */
    END CASE.
    fSetSaveButton (YES).
    fShowQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetSubjectParamSet C-Win 
PROCEDURE pSetSubjectParamSet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiSubjectID  AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiParamSetID AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCol        AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdRow        AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bttSubjectParamSet FOR ttSubjectParamSet.
    
    FIND FIRST bttSubjectParamSet
         WHERE bttSubjectParamSet.subjectID  EQ ipiSubjectID
           AND bttSubjectParamSet.paramSetID EQ ipiParamSetID
         NO-ERROR.
    IF AVAILABLE bttSubjectParamSet THEN DO TRANSACTION:
        ASSIGN
            bttSubjectParamSet.setCol = ipdCol
            bttSubjectParamSet.setRow = ipdRow
            .
        {AOA/includes/dynSubjctSave.i "ParamSet"}
    END. /* if avail */

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
        HIDE FRAME viewFrame.
        HIDE FRAME {&FRAME-NAME}.
        IF {&WINDOW-NAME}:HEIGHT LT 28.57 THEN
        {&WINDOW-NAME}:HEIGHT = 28.57.
        IF {&WINDOW-NAME}:WIDTH  LT 160   THEN
        {&WINDOW-NAME}:WIDTH  = 160.
        ASSIGN
            /* view frame */
            FRAME viewFrame:COL                 = 1
            FRAME viewFrame:ROW                 = 1
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
            /* view frame */
            FRAME viewFrame:COL                 = FRAME {&FRAME-NAME}:WIDTH
                                                - FRAME viewFrame:WIDTH  + 1
            FRAME viewFrame:ROW                 = FRAME {&FRAME-NAME}:HEIGHT
                                                - FRAME viewFrame:HEIGHT + 1
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
                hQueryBrowse:HEIGHT = FRAME resultsFrame:HEIGHT - .1
                hQueryBrowse:WIDTH  = FRAME resultsFrame:WIDTH - .32
                .
        END. /* if valid-handle */
    END. /* do with */
    DO WITH FRAME paramFrame:
        ASSIGN
            FRAME outputFrame:COL = 1
            btnCloseParam:COL     = FRAME paramFrame:WIDTH
                                  - btnCloseParam:WIDTH
                                  .
        btnCloseParam:MOVE-TO-TOP().
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
    
    IF ipcTableDB NE "" AND ipcTableName NE "" THEN DO:
        CREATE ALIAS "dictdb" FOR DATABASE VALUE(ipcTableDB).
        RUN nosweat/indxlist.p (ipcTableName, OUTPUT cIndexList).
    END. /* if not blank */
    
    RETURN cIndexList.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetVariable C-Win 
FUNCTION fGetVariable RETURNS CHARACTER
  (ipParamName AS CHARACTER, ipParamDataType AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cPreFix   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cVariable AS CHARACTER NO-UNDO.

    CASE ipParamDataType:
        WHEN "Character" THEN
        cPreFix = "c".
        WHEN "Date" THEN
        cPreFix = "dt".
        WHEN "Decimal" THEN
        cPreFix = "d".
        WHEN "Integer" THEN
        cPreFix = "i".
        WHEN "Logical" THEN
        cPreFix = "l".
    END CASE.
    ASSIGN
        cVariable = ipParamName
        SUBSTRING(cVariable,1,1) = CAPS(SUBSTRING(cVariable,1,1))
        cVariable = cPreFix + cVariable
        .
    RETURN cVariable.

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
        WHERE ttSubjectTable.subjectID EQ dynSubject.subjectID
           BY ttSubjectTable.sortOrder
        :
        cQueryStr = cQueryStr + " "
                  + ttSubjectTable.tableFind + " "
                  + ttSubjectTable.tableName + " "
                  .
        FOR EACH ttSubjectWhere
            WHERE ttSubjectWhere.subjectID  EQ ttSubjectTable.subjectID
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
            lSave             = iplSave
            btnSave:HIDDEN    = NOT lSave
            btnSave:SENSITIVE = lSave
            btnUndo:HIDDEN    = btnSave:HIDDEN
            btnUndo:SENSITIVE = btnSave:SENSITIVE
            BROWSE subjectBrowse:SENSITIVE = btnSave:HIDDEN
            subjectSearch:SENSITIVE  = btnSave:HIDDEN
            subjectMatches:SENSITIVE = btnSave:HIDDEN
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
    DO WITH FRAME outputFrame:
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
        .
    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

