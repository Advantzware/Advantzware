&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
  Name         : wDataDigger.w
  Description  : Main program for DataDigger
  ---------------------------------------------------------------------- 
  15-10-2009 pti Created
  ----------------------------------------------------------------------*/

create widget-pool.

/* ***************************  Definitions  ************************** */

/* Buildnr, temp-tables and forward defs */
{ util\DataDigger\DataDigger.i }

define temp-table ttView no-undo
  field cColumnName as character
  field iFieldNr  as integer  
  field iColumnNr as integer  
  field cValue    as character format 'x(20)'
  .
define temp-table ttColumnWidth no-undo
  field cColumnName as character
  field iColumnNr   as integer
  field iWidth      as integer
  . 

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
define variable gcCurrentTable             as character   no-undo.
define variable gcCurrentDatabase          as character   no-undo.

define variable gcFieldFilterList          as character   no-undo.
define variable gcDataBrowseColumnNames    as character   no-undo.
define variable gcDataBrowseColumns        as character   no-undo.
define variable gcFieldBrowseColumnHandles as character   no-undo.
define variable gcFieldBrowseColumnNames   as character   no-undo.
define variable gcIndexBrowseColumnHandles as character   no-undo.
define variable gcQueryEditorState         as character   no-undo.
define variable ghDataBrowse               as handle      no-undo.
define variable ghFieldBrowse              as handle      no-undo.
define variable ghLastFilterField          as handle      no-undo.
define variable ghLastIndexFilter          as handle      no-undo.
define variable gcLastDataField            as character   no-undo. 
define variable ghNameColumn               as handle      no-undo.
define variable giCurrentPage              as integer     no-undo. /* 1=fields 2=indexes */.
define variable giQueryPointer             as integer     no-undo.
define variable giTimeLastTableChange      as integer     no-undo. 
define variable giWindowLock               as integer     no-undo.
define variable glPendingValueChanged      as logical     no-undo.
define variable glRowEditActive            as logical     no-undo.

define variable giTimeLastFilterChange     as integer     no-undo.
define variable glPendingFilterChange      as logical     no-undo.
define variable gcLastFilterChangeBrowse   as character   no-undo.

define variable ghOverlayField             as handle      no-undo extent 500.
define variable gcRecordMode               as character   no-undo.
define variable hDiggerLib   as handle      no-undo.

&global-define numDigits   6
&global-define digitWidth  6
&global-define digitHeight 15
&global-define marginHor   3
&global-define marginVer   3

define variable ghNewDigit                 as handle      no-undo extent {&numDigits}.
define variable ghOldDigit                 as handle      no-undo extent {&numDigits}.
define variable glDebugMode                as logical     no-undo initial NO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frMain
&Scoped-define BROWSE-NAME brFields

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttField ttIndex ttTable

/* Definitions for BROWSE brFields                                      */
&Scoped-define FIELDS-IN-QUERY-brFields ttField.lShow ttField.iOrder ttField.cFieldName (if ttField.iExtent > 0 then substitute('&1[&2]', ttField.cDataType, ttField.iExtent) else ttField.cDataType ) @ ttField.cDataType ttField.cFormat ttField.cLabel   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brFields ttField.lShow  ttField.cFormat   
&Scoped-define ENABLED-TABLES-IN-QUERY-brFields ttField
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brFields ttField
&Scoped-define SELF-NAME brFields
&Scoped-define QUERY-STRING-brFields FOR EACH ttField
&Scoped-define OPEN-QUERY-brFields OPEN QUERY {&SELF-NAME} FOR EACH ttField.
&Scoped-define TABLES-IN-QUERY-brFields ttField
&Scoped-define FIRST-TABLE-IN-QUERY-brFields ttField


/* Definitions for BROWSE brIndexes                                     */
&Scoped-define FIELDS-IN-QUERY-brIndexes cIndexName cIndexFlags cIndexFields   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brIndexes   
&Scoped-define SELF-NAME brIndexes
&Scoped-define QUERY-STRING-brIndexes FOR EACH ttIndex
&Scoped-define OPEN-QUERY-brIndexes OPEN QUERY {&SELF-NAME} FOR EACH ttIndex.
&Scoped-define TABLES-IN-QUERY-brIndexes ttIndex
&Scoped-define FIRST-TABLE-IN-QUERY-brIndexes ttIndex


/* Definitions for BROWSE brTables                                      */
&Scoped-define FIELDS-IN-QUERY-brTables ttTable.cTableName ttTable.cDatabase ttTable.iNumQueries ttTable.tLastUsed   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brTables   
&Scoped-define SELF-NAME brTables
&Scoped-define QUERY-STRING-brTables FOR EACH ttTable
&Scoped-define OPEN-QUERY-brTables OPEN QUERY {&SELF-NAME} FOR EACH ttTable.
&Scoped-define TABLES-IN-QUERY-brTables ttTable
&Scoped-define FIRST-TABLE-IN-QUERY-brTables ttTable


/* Definitions for FRAME frMain                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-frMain ~
    ~{&OPEN-QUERY-brFields}~
    ~{&OPEN-QUERY-brIndexes}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnTabFields rctData rctQuery rctEdit ~
rcCounter btnDataDigger btnTools btnHelp btnQueryTester fiOrderFilter ~
fiNameFilter fiTypeFilter fiFormatFilter fiLabelFilter btnClearFieldFilter ~
btnFieldFilter fiIndexNameFilter fiFlagsFilter fiFieldsFilter btnTabIndexes ~
btnClearIndexFilter btnIndexFilter tgSelAll brFields btnAddFilter ~
btnAddFilter2 brIndexes fiTableFilter cbDatabaseFilter fiNumQueriesFilter ~
fiLastUsedFilter btnClearTableFilter btnTableFilter btnEditFilter ~
btnEditFilter2 brTables btnMoveTop btnMoveUp btnReset btnMoveDown ~
btnMoveBottom fiTableDesc btnWhere btnClear btnPrevQuery btnQueries ~
btnNextQuery btnClipboard ficWhere btnClearDataFilter btnDataFilter btnAdd ~
btnEdit btnDelete btnView btnDump btnLoad fiNumresults 
&Scoped-Define DISPLAYED-OBJECTS fiOrderFilter fiNameFilter fiTypeFilter ~
fiFormatFilter fiLabelFilter fiIndexNameFilter fiFlagsFilter fiFieldsFilter ~
tgSelAll fiTableFilter cbDatabaseFilter fiNumQueriesFilter fiLastUsedFilter ~
fiTableDesc ficWhere fiIndexInfo fiNumresults 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 btnAnd rctQueryButtons cbAndOr cbFields cbOperator ~
ficValue btnInsert btnBegins btnBracket btnContains btnEq btnGT btnLT ~
btnMatches btnNE btnOr btnQt btnToday 
&Scoped-define List-2 rcFieldFilter fiOrderFilter fiNameFilter fiTypeFilter ~
fiFormatFilter fiLabelFilter btnClearFieldFilter btnFieldFilter tgSelAll ~
brFields btnAddFilter btnAddFilter2 btnClearTableFilter btnEditFilter ~
btnEditFilter2 btnRemoveFilter btnRemoveFilter2 btnMoveTop btnMoveUp ~
btnReset btnMoveDown btnMoveBottom btnClearDataFilter btnDataFilter 
&Scoped-define List-3 rcIndexFilter fiIndexNameFilter fiFlagsFilter ~
fiFieldsFilter btnClearIndexFilter btnIndexFilter btnAddFilter2 brIndexes ~
btnEditFilter2 btnRemoveFilter2 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD enableTimers C-Win 
FUNCTION enableTimers RETURNS LOGICAL
  ( input plTimersEnabled as logical )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getActiveQueryEditor C-Win 
FUNCTION getActiveQueryEditor RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCurrentDatabase C-Win 
FUNCTION getCurrentDatabase RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCurrentTable C-Win 
FUNCTION getCurrentTable RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDataQuery C-Win 
FUNCTION getDataQuery RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFieldList C-Win 
FUNCTION getFieldList returns character
  ( pcSortBy as character )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFilterQuery C-Win 
FUNCTION getFilterQuery RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getProgramDir Procedure 
FUNCTION getProgramDir RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTempDir Procedure 
FUNCTION getTempDir RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getQueryFromFields C-Win 
FUNCTION getQueryFromFields returns character
  ( input pcFieldList as character ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSelectedFields C-Win 
FUNCTION getSelectedFields returns character
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD saveSelectedFields C-Win 
FUNCTION saveSelectedFields RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setCurrentTable C-Win 
FUNCTION setCurrentTable returns logical
  ( pcTableName as character )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setNumRecords C-Win 
FUNCTION setNumRecords returns logical
  ( input piNumRecords    as integer
  , input plCountComplete as logical 
  , input piQueryMSec     as integer)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( piPointerChange as integer )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQueryEditor C-Win 
FUNCTION setQueryEditor RETURNS LOGICAL
  ( pcQueryEditorState as character )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setUpdatePanel C-Win 
FUNCTION setUpdatePanel RETURNS LOGICAL
  ( input pcMode as character )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setWindowFreeze C-Win 
FUNCTION setWindowFreeze RETURNS LOGICAL
  ( plWindowsLocked as logical )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-brTables 
       MENU-ITEM m_Quick_Connect LABEL "Quick Connect" 
       MENU-ITEM m_Disconnect   LABEL "Disconnect"    
       MENU-ITEM m_Manage_Favorites LABEL "Manage Favorites"
       MENU-ITEM m_Show_hidden_tables LABEL "Show &hidden tables"
              TOGGLE-BOX
       RULE.

DEFINE MENU POPUP-MENU-btnView 
       MENU-ITEM m_View_as_text LABEL "View as TEXT"  
       MENU-ITEM m_View_as_HTML LABEL "View as HTML"  
       MENU-ITEM m_View_as_Excel LABEL "View as Excel" .


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE FilterTimer AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chFilterTimer AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAdd 
     LABEL "&Add" 
     SIZE-PIXELS 45 BY 23.

DEFINE BUTTON btnAddFilter 
     LABEL "<-" 
     CONTEXT-HELP-ID 1010
     SIZE-PIXELS 23 BY 23 TOOLTIP "add field to table filter".

DEFINE BUTTON btnAddFilter2 
     LABEL "<-" 
     CONTEXT-HELP-ID 1010
     SIZE-PIXELS 23 BY 23 TOOLTIP "add fields to table filter".

DEFINE BUTTON btnClear 
     LABEL "&C" 
     SIZE-PIXELS 20 BY 23 TOOLTIP "clear the where field".

DEFINE BUTTON btnClearDataFilter 
     LABEL "C" 
     SIZE-PIXELS 20 BY 21 TOOLTIP "SHIFT-DEL".

DEFINE BUTTON btnClearFieldFilter 
     LABEL "C" 
     CONTEXT-HELP-ID 280
     SIZE-PIXELS 20 BY 21 TOOLTIP "SHIFT-DEL".

DEFINE BUTTON btnClearIndexFilter 
     LABEL "C" 
     CONTEXT-HELP-ID 960
     SIZE-PIXELS 20 BY 21 TOOLTIP "SHIFT-DEL".

DEFINE BUTTON btnClearTableFilter 
     LABEL "C" 
     CONTEXT-HELP-ID 950
     SIZE-PIXELS 20 BY 21 TOOLTIP "SHIFT-DEL".

DEFINE BUTTON btnClipboard 
     LABEL "Cp" 
     SIZE-PIXELS 20 BY 23 TOOLTIP "copy the expression to the clipboard".

DEFINE BUTTON btnDataDigger 
     LABEL "DD" 
     SIZE-PIXELS 30 BY 30 TOOLTIP "start a new instance of DataDigger".

DEFINE BUTTON btnDataFilter 
     LABEL "Y" 
     SIZE-PIXELS 20 BY 21.

DEFINE BUTTON btnDelete 
     LABEL "&Delete" 
     SIZE-PIXELS 45 BY 23.

DEFINE BUTTON btnDump 
     LABEL "&Dump" 
     SIZE-PIXELS 75 BY 23 TOOLTIP "dump all data".

DEFINE BUTTON btnEdit 
     LABEL "&Edit" 
     SIZE-PIXELS 45 BY 23 TOOLTIP "edit the selected records".

DEFINE BUTTON btnEditFilter 
     LABEL "..." 
     CONTEXT-HELP-ID 1010
     SIZE-PIXELS 23 BY 23 TOOLTIP "edit table filter".

DEFINE BUTTON btnEditFilter2 
     LABEL "..." 
     CONTEXT-HELP-ID 1010
     SIZE-PIXELS 23 BY 23 TOOLTIP "edit table filter".

DEFINE BUTTON btnFieldFilter 
     LABEL "Y" 
     CONTEXT-HELP-ID 280
     SIZE-PIXELS 20 BY 21.

DEFINE BUTTON btnHelp 
     LABEL "Help" 
     SIZE-PIXELS 30 BY 30 TOOLTIP "get help about the DataDigger window".

DEFINE BUTTON btnIndexFilter 
     LABEL "Y" 
     CONTEXT-HELP-ID 960
     SIZE-PIXELS 20 BY 21.

DEFINE BUTTON btnLoad 
     LABEL "&Load" 
     SIZE-PIXELS 75 BY 23 TOOLTIP "load data".

DEFINE BUTTON btnMoveBottom 
     LABEL "Btm" 
     SIZE-PIXELS 23 BY 23 TOOLTIP "move selected field to bottom".

DEFINE BUTTON btnMoveDown 
     LABEL "Dn" 
     SIZE-PIXELS 23 BY 23 TOOLTIP "move selected field down".

DEFINE BUTTON btnMoveTop 
     LABEL "Top" 
     SIZE-PIXELS 23 BY 23 TOOLTIP "move selected field to top".

DEFINE BUTTON btnMoveUp 
     LABEL "Up" 
     SIZE-PIXELS 23 BY 23 TOOLTIP "move selected field up".

DEFINE BUTTON btnNextQuery 
     LABEL ">" 
     SIZE-PIXELS 12 BY 23 TOOLTIP "next query".

DEFINE BUTTON btnPrevQuery 
     LABEL "<" 
     SIZE-PIXELS 12 BY 23 TOOLTIP "previous query".

DEFINE BUTTON btnQueries 
     LABEL "&Q" 
     SIZE-PIXELS 20 BY 23 TOOLTIP "show previous queries on this table".

DEFINE BUTTON btnQueryTester 
     LABEL "QT" 
     SIZE-PIXELS 30 BY 30 TOOLTIP "Start 'QueryTester'".

DEFINE BUTTON btnRemoveFilter 
     LABEL "->" 
     CONTEXT-HELP-ID 1010
     SIZE-PIXELS 23 BY 23 TOOLTIP "remove field from table filter".

DEFINE BUTTON btnRemoveFilter2 
     LABEL "->" 
     CONTEXT-HELP-ID 1010
     SIZE-PIXELS 23 BY 23 TOOLTIP "remove fields from table filter".

DEFINE BUTTON btnReset 
     LABEL "R" 
     SIZE-PIXELS 23 BY 23 TOOLTIP "reset default ordering".

DEFINE BUTTON btnTabFields  NO-FOCUS FLAT-BUTTON
     LABEL "Fld" 
     CONTEXT-HELP-ID 270
     SIZE-PIXELS 25 BY 85 TOOLTIP "CTRL-1 to jump, CTRL-TAB to switch".

DEFINE BUTTON btnTabIndexes  NO-FOCUS FLAT-BUTTON
     LABEL "Idx" 
     CONTEXT-HELP-ID 270
     SIZE-PIXELS 25 BY 85 TOOLTIP "CTRL-2 to jump, CTRL-TAB to switch".

DEFINE BUTTON btnTableFilter 
     LABEL "Y" 
     CONTEXT-HELP-ID 950
     SIZE-PIXELS 20 BY 21.

DEFINE BUTTON btnTools 
     LABEL "Tools" 
     SIZE-PIXELS 30 BY 30 TOOLTIP "tools and settings".

DEFINE BUTTON btnView 
     LABEL "&View" 
     SIZE-PIXELS 75 BY 23 TOOLTIP "right click to set type of view".

DEFINE BUTTON btnViewData 
     LABEL "->" 
     SIZE-PIXELS 20 BY 23 TOOLTIP "execute the query".

DEFINE BUTTON btnWhere 
     LABEL "&Where" 
     SIZE-PIXELS 50 BY 24 TOOLTIP "show expanded queryeditor".

DEFINE VARIABLE cbDatabaseFilter AS CHARACTER FORMAT "X(256)":U 
     CONTEXT-HELP-ID 950
     VIEW-AS COMBO-BOX SORT INNER-LINES 10
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE-PIXELS 79 BY 21 NO-UNDO.

DEFINE VARIABLE ficWhere AS CHARACTER 
     CONTEXT-HELP-ID 110
     VIEW-AS EDITOR NO-WORD-WRAP
     SIZE-PIXELS 585 BY 21 TOOLTIP "alt-cursor-up / down to view/hide query editor"
     FONT 2 NO-UNDO.

DEFINE VARIABLE fiFieldsFilter AS CHARACTER FORMAT "X(256)":U INITIAL "Fields" 
     CONTEXT-HELP-ID 960
     VIEW-AS FILL-IN 
     SIZE-PIXELS 140 BY 21
     FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE fiFlagsFilter AS CHARACTER FORMAT "X(256)":U INITIAL "Flags" 
     CONTEXT-HELP-ID 960
     VIEW-AS FILL-IN 
     SIZE-PIXELS 55 BY 21
     FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE fiFormatFilter AS CHARACTER FORMAT "X(256)":U INITIAL "Format" 
     CONTEXT-HELP-ID 280
     VIEW-AS FILL-IN 
     SIZE-PIXELS 75 BY 21
     FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE fiIndexInfo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE-PIXELS 325 BY 21
     FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE fiIndexNameFilter AS CHARACTER FORMAT "X(256)":U INITIAL "Index Name" 
     CONTEXT-HELP-ID 960
     VIEW-AS FILL-IN 
     SIZE-PIXELS 75 BY 21
     FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE fiLabelFilter AS CHARACTER FORMAT "X(256)":U INITIAL "Label" 
     CONTEXT-HELP-ID 280
     VIEW-AS FILL-IN 
     SIZE-PIXELS 85 BY 21
     FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE fiLastUsedFilter AS DATETIME FORMAT "99/99/9999 HH:MM:SS":U 
     CONTEXT-HELP-ID 950
     VIEW-AS FILL-IN 
     SIZE-PIXELS 15 BY 21
     FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE fiNameFilter AS CHARACTER FORMAT "X(256)":U INITIAL "Name" 
     CONTEXT-HELP-ID 280
     VIEW-AS FILL-IN 
     SIZE-PIXELS 185 BY 21
     FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE fiNumQueriesFilter AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     CONTEXT-HELP-ID 950
     VIEW-AS FILL-IN 
     SIZE-PIXELS 15 BY 21
     FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE fiNumresults AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE-PIXELS 70 BY 13 TOOLTIP "nr of results of the query. Double click to count" NO-UNDO.

DEFINE VARIABLE fiOrderFilter AS CHARACTER FORMAT "X(256)":U INITIAL "Order" 
     CONTEXT-HELP-ID 280
     VIEW-AS FILL-IN 
     SIZE-PIXELS 35 BY 21
     FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE fiTableDesc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE-PIXELS 220 BY 21 NO-UNDO.

DEFINE VARIABLE fiTableFilter AS CHARACTER FORMAT "X(256)":U INITIAL "Table filter" 
     CONTEXT-HELP-ID 950
     VIEW-AS FILL-IN 
     SIZE-PIXELS 70 BY 21
     FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE fiTypeFilter AS CHARACTER FORMAT "X(256)":U INITIAL "Type" 
     CONTEXT-HELP-ID 280
     VIEW-AS FILL-IN 
     SIZE-PIXELS 55 BY 21
     FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE fiWarning AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE-PIXELS 45 BY 21
     BGCOLOR 14 FGCOLOR 12  NO-UNDO.

DEFINE RECTANGLE rcCounter
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 45 BY 20.

DEFINE RECTANGLE rcFieldFilter
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 101.2 BY 9.62
     BGCOLOR 12 FGCOLOR 12 .

DEFINE RECTANGLE rcIndexFilter
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 63 BY 9.57
     BGCOLOR 12 FGCOLOR 12 .

DEFINE RECTANGLE rcTableFilter
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 45.2 BY 8.1
     BGCOLOR 12 FGCOLOR 12 .

DEFINE RECTANGLE rctData
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 790 BY 271.

DEFINE RECTANGLE rctEdit
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 430 BY 35.

DEFINE RECTANGLE rctQuery
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 789 BY 252
     BGCOLOR 3 .

DEFINE VARIABLE tgDebugMode AS LOGICAL INITIAL yes 
     LABEL "debugmode" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .71 TOOLTIP "debugging mode".

DEFINE VARIABLE tgSelAll AS LOGICAL INITIAL yes 
     LABEL "" 
     CONTEXT-HELP-ID 280
     VIEW-AS TOGGLE-BOX
     SIZE-PIXELS 14 BY 15 TOOLTIP "toggle to (de)select all fields" NO-UNDO.

DEFINE BUTTON btnAbout 
     LABEL "Que" 
     SIZE-PIXELS 30 BY 30 TOOLTIP "about the DataDigger".

DEFINE BUTTON btnAbout-txt  NO-FOCUS FLAT-BUTTON
     LABEL "About DataDigger" 
     SIZE-PIXELS 144 BY 30
     FONT 0.

DEFINE BUTTON btnChangeLog 
     LABEL "?" 
     SIZE-PIXELS 30 BY 30 TOOLTIP "watskeburt".

DEFINE BUTTON btnChangeLog-txt  NO-FOCUS FLAT-BUTTON
     LABEL "Changelog / help" 
     SIZE-PIXELS 144 BY 30
     FONT 0.

DEFINE BUTTON btnDump-2 
     LABEL "&Dmp" 
     SIZE-PIXELS 30 BY 30 TOOLTIP "dump all data".

DEFINE BUTTON btnDump-txt  NO-FOCUS FLAT-BUTTON
     LABEL "Dump data" 
     SIZE-PIXELS 144 BY 30
     FONT 0.

DEFINE BUTTON btnFavorites 
     LABEL "Fav" 
     SIZE-PIXELS 30 BY 30 TOOLTIP "favourites".

DEFINE BUTTON btnFavorites-txt  NO-FOCUS FLAT-BUTTON
     LABEL "Connections" 
     SIZE-PIXELS 144 BY 30
     FONT 0.

DEFINE BUTTON btnLoad-2 
     LABEL "&Load" 
     SIZE-PIXELS 30 BY 30 TOOLTIP "load data".

DEFINE BUTTON btnLoad-txt  NO-FOCUS FLAT-BUTTON
     LABEL "Load Data" 
     SIZE-PIXELS 144 BY 30 TOOLTIP "load data"
     FONT 0.

DEFINE BUTTON btnQueries-3 
     LABEL "&Q" 
     SIZE-PIXELS 30 BY 30 TOOLTIP "show previous queries on this table".

DEFINE BUTTON btnQueries-txt  NO-FOCUS FLAT-BUTTON
     LABEL "Manage queries" 
     SIZE-PIXELS 144 BY 30
     FONT 0.

DEFINE BUTTON btnSettings 
     LABEL "INI" 
     SIZE-PIXELS 30 BY 30 TOOLTIP "edit your settings file".

DEFINE BUTTON btnSettings-txt  NO-FOCUS FLAT-BUTTON
     LABEL "Settings" 
     SIZE-PIXELS 144 BY 30
     FONT 0.

DEFINE BUTTON btnAnd  NO-FOCUS
     LABEL "and" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 30 BY 21 TOOLTIP "insert this text into the where field".

DEFINE BUTTON btnBegins  NO-FOCUS
     LABEL "begins" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 60 BY 21 TOOLTIP "insert this text into the where field".

DEFINE BUTTON btnBracket  NO-FOCUS
     LABEL "()" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 30 BY 21 TOOLTIP "insert this text into the where field"
     FONT 0.

DEFINE BUTTON btnCancel-2 DEFAULT 
     LABEL "Cancel" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 75 BY 24
     BGCOLOR 8 .

DEFINE BUTTON btnClear-2 
     LABEL "&C" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 20 BY 23 TOOLTIP "clear the where field".

DEFINE BUTTON btnClipboard-2 
     LABEL "Cp" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 20 BY 23 TOOLTIP "copy the expression to the clipboard".

DEFINE BUTTON btnContains  NO-FOCUS
     LABEL "contains" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 60 BY 21 TOOLTIP "insert this text into the where field".

DEFINE BUTTON btnEq  NO-FOCUS
     LABEL "=" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 30 BY 21 TOOLTIP "insert this text into the where field".

DEFINE BUTTON btnGT  NO-FOCUS
     LABEL ">" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 30 BY 21 TOOLTIP "insert this text into the where field".

DEFINE BUTTON btnInsert 
     LABEL "+" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 20 BY 23 TOOLTIP "insert the expression into the where field".

DEFINE BUTTON btnLT  NO-FOCUS
     LABEL "<" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 30 BY 21 TOOLTIP "insert this text into the where field".

DEFINE BUTTON btnMatches  NO-FOCUS
     LABEL "matches" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 60 BY 21 TOOLTIP "insert this text into the where field".

DEFINE BUTTON btnNE  NO-FOCUS
     LABEL "<>" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 30 BY 21 TOOLTIP "insert this text into the where field".

DEFINE BUTTON btnNextQuery-2 
     LABEL ">" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 12 BY 23 TOOLTIP "next query".

DEFINE BUTTON btnOK AUTO-GO DEFAULT 
     LABEL "OK" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 75 BY 24
     BGCOLOR 8 .

DEFINE BUTTON btnOr  NO-FOCUS
     LABEL "or" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 30 BY 21 TOOLTIP "insert this text into the where field".

DEFINE BUTTON btnPrevQuery-2 
     LABEL "<" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 12 BY 23 TOOLTIP "previous query".

DEFINE BUTTON btnQt  NO-FOCUS
     LABEL "~"~"" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 30 BY 21 TOOLTIP "insert this text into the where field"
     FONT 0.

DEFINE BUTTON btnQueries-2 
     LABEL "&Q" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 20 BY 23 TOOLTIP "show previous queries on this table".

DEFINE BUTTON btnToday  NO-FOCUS
     LABEL "today" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 60 BY 21 TOOLTIP "insert this text into the where field".

DEFINE BUTTON btnViewData-2 
     LABEL "->" 
     CONTEXT-HELP-ID 1050
     SIZE-PIXELS 20 BY 23 TOOLTIP "execute the query".

DEFINE VARIABLE cbAndOr AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Where" 
     CONTEXT-HELP-ID 1050
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","AND","OR" 
     DROP-DOWN-LIST
     SIZE-PIXELS 40 BY 21 TOOLTIP "preceding AND or OR for the expression"
     FONT 2 NO-UNDO.

DEFINE VARIABLE cbFields AS CHARACTER FORMAT "X(256)":U 
     CONTEXT-HELP-ID 1050
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE-PIXELS 210 BY 21 TOOLTIP "field used in the expression"
     FONT 2 NO-UNDO.

DEFINE VARIABLE cbOperator AS CHARACTER FORMAT "X(256)":U 
     CONTEXT-HELP-ID 1050
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "","=","<>",">",">=","<","<=","begins","matches","contains" 
     DROP-DOWN-LIST
     SIZE-PIXELS 85 BY 21 TOOLTIP "operator used in the expression"
     FONT 2 NO-UNDO.

DEFINE VARIABLE ficWhere2 AS CHARACTER 
     CONTEXT-HELP-ID 1050
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE-PIXELS 525 BY 170 TOOLTIP "alt-cursor-up / down to view/hide query editor"
     FONT 2 NO-UNDO.

DEFINE VARIABLE ficValue AS CHARACTER FORMAT "X(256)":U 
     CONTEXT-HELP-ID 1050
     VIEW-AS FILL-IN 
     SIZE-PIXELS 210 BY 23 TOOLTIP "the literal value for the expression"
     FONT 2 NO-UNDO.

DEFINE RECTANGLE rctQueryButtons
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 610 BY 180.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brFields FOR 
      ttField SCROLLING.

DEFINE QUERY brIndexes FOR 
      ttIndex SCROLLING.

DEFINE QUERY brTables FOR 
      ttTable SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brFields C-Win _FREEFORM
  QUERY brFields DISPLAY
      ttField.lShow view-as toggle-box 
  ttField.iOrder     
  ttField.cFieldName 
  (if ttField.iExtent > 0 
    then substitute('&1[&2]', ttField.cDataType, ttField.iExtent) 
    else ttField.cDataType ) @ ttField.cDataType  
  ttField.cFormat    
  ttField.cLabel
  enable 
  ttField.lShow 
  ttField.cFormat
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-VALIDATE
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 100 BY 9
          &ELSE SIZE-PIXELS 500 BY 196 &ENDIF ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN TOOLTIP "fields of selected table"
         CONTEXT-HELP-ID 80.

DEFINE BROWSE brIndexes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brIndexes C-Win _FREEFORM
  QUERY brIndexes DISPLAY
      cIndexName   
cIndexFlags 
cIndexFields
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-VALIDATE
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 62 BY 9
          &ELSE SIZE-PIXELS 308 BY 193 &ENDIF FIT-LAST-COLUMN TOOLTIP "indexes of the table"
         CONTEXT-HELP-ID 90.

DEFINE BROWSE brTables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brTables C-Win _FREEFORM
  QUERY brTables DISPLAY
      ttTable.cTableName  
      ttTable.cDatabase   
      ttTable.iNumQueries 
      ttTable.tLastUsed
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 44 BY 7
          &ELSE SIZE-PIXELS 220 BY 144 &ENDIF FIT-LAST-COLUMN
         CONTEXT-HELP-ID 70.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frMain
     btnTabFields AT Y 45 X 230 WIDGET-ID 156
     btnDataDigger AT Y 5 X 5 WIDGET-ID 126
     btnTools AT Y 5 X 35 WIDGET-ID 264
     btnHelp AT Y 5 X 65 WIDGET-ID 260
     btnQueryTester AT Y 5 X 95 WIDGET-ID 240
     fiOrderFilter AT Y 5 X 265 COLON-ALIGNED NO-LABEL WIDGET-ID 48
     fiNameFilter AT Y 5 X 300 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     fiTypeFilter AT Y 5 X 485 COLON-ALIGNED NO-LABEL WIDGET-ID 52
     fiFormatFilter AT Y 5 X 540 COLON-ALIGNED NO-LABEL WIDGET-ID 54
     fiLabelFilter AT Y 5 X 615 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     btnClearFieldFilter AT Y 5 X 710 WIDGET-ID 232
     btnFieldFilter AT Y 5 X 731 WIDGET-ID 234
     fiIndexNameFilter AT Y 5 X 785 COLON-ALIGNED NO-LABEL WIDGET-ID 168
     fiFlagsFilter AT Y 5 X 860 COLON-ALIGNED NO-LABEL WIDGET-ID 164
     fiFieldsFilter AT Y 5 X 915 COLON-ALIGNED NO-LABEL WIDGET-ID 166
     btnTabIndexes AT Y 130 X 230 WIDGET-ID 158
     btnClearIndexFilter AT Y 5 X 1065 WIDGET-ID 160
     btnIndexFilter AT Y 5 X 1086 WIDGET-ID 162
     tgSelAll AT Y 7 X 259 WIDGET-ID 6
     brFields AT Y 27 X 255 WIDGET-ID 100
     btnAddFilter AT Y 27 X 760 WIDGET-ID 242
     btnAddFilter2 AT Y 27 X 1110 WIDGET-ID 246
     brIndexes AT Y 28 X 799 WIDGET-ID 200
     fiTableFilter AT Y 37 X 5 NO-LABEL
     cbDatabaseFilter AT Y 37 X 66 COLON-ALIGNED NO-LABEL
     fiNumQueriesFilter AT Y 37 X 155 NO-LABEL WIDGET-ID 204
     fiLastUsedFilter AT Y 37 X 170 NO-LABEL WIDGET-ID 220
     btnClearTableFilter AT Y 37 X 185 WIDGET-ID 222
     btnTableFilter AT Y 37 X 205 WIDGET-ID 38
     btnEditFilter AT Y 49 X 760 WIDGET-ID 244
     btnEditFilter2 AT Y 49 X 1110 WIDGET-ID 252
     brTables AT Y 59 X 5 WIDGET-ID 300
     btnRemoveFilter AT Y 71 X 760 WIDGET-ID 250
     btnRemoveFilter2 AT Y 71 X 1110 WIDGET-ID 248
     btnMoveTop AT Y 112 X 760 WIDGET-ID 198
     btnMoveUp AT Y 134 X 760 WIDGET-ID 192
     btnReset AT Y 156 X 760 WIDGET-ID 196
     btnMoveDown AT Y 178 X 760 WIDGET-ID 194
     btnMoveBottom AT Y 200 X 760 WIDGET-ID 200
     fiTableDesc AT Y 202 X 5 NO-LABEL WIDGET-ID 90
     btnWhere AT Y 225 X 5 WIDGET-ID 236
     btnViewData AT Y 225 X 651
     btnClear AT Y 225 X 671 WIDGET-ID 30
     btnPrevQuery AT Y 225 X 691 WIDGET-ID 212
     btnQueries AT Y 225 X 703 WIDGET-ID 190
     btnNextQuery AT Y 225 X 723 WIDGET-ID 214
     btnClipboard AT Y 225 X 735 WIDGET-ID 178
     ficWhere AT Y 227 X 60 NO-LABEL
     btnClearDataFilter AT Y 265 X 10 WIDGET-ID 76
     btnDataFilter AT Y 265 X 30 WIDGET-ID 58
     tgDebugMode AT ROW 13.86 COL 19 WIDGET-ID 238
     fiWarning AT Y 395 X 725 COLON-ALIGNED NO-LABEL WIDGET-ID 172
     btnAdd AT Y 535 X 5
     btnEdit AT Y 535 X 55
     btnDelete AT Y 535 X 105
     btnView AT Y 535 X 190 WIDGET-ID 4
     btnDump AT Y 535 X 270
     btnLoad AT Y 535 X 350 WIDGET-ID 224
     fiIndexInfo AT Y 537 X 455 COLON-ALIGNED NO-LABEL WIDGET-ID 216
     fiNumresults AT Y 520 X 685 COLON-ALIGNED NO-LABEL WIDGET-ID 210
     "#" VIEW-AS TEXT
          SIZE-PIXELS 10 BY 13 AT Y 13 X 150 WIDGET-ID 266
          FGCOLOR 7 
     rctData AT Y 255 X 0
     rctQuery AT Y 0 X 0
     rctEdit AT Y 530 X 0
     rcCounter AT Y 10 X 160 WIDGET-ID 82
     rcTableFilter AT ROW 3.67 COL 1.4 WIDGET-ID 254
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT X 0 Y 0
         SIZE-PIXELS 1448 BY 573.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME frMain
     rcFieldFilter AT ROW 2.14 COL 51.4 WIDGET-ID 256
     rcIndexFilter AT ROW 2.14 COL 160 WIDGET-ID 258
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT X 0 Y 0
         SIZE-PIXELS 1448 BY 573.

DEFINE FRAME frWhere
     btnAnd AT Y 98 X 15 WIDGET-ID 22
     cbAndOr AT Y 5 X 35 COLON-ALIGNED WIDGET-ID 10
     cbFields AT Y 5 X 76 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     cbOperator AT Y 5 X 286 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     ficValue AT Y 5 X 371 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     btnInsert AT Y 5 X 595 WIDGET-ID 18
     ficWhere2 AT Y 35 X 86 NO-LABEL WIDGET-ID 130
     btnViewData-2 AT Y 210 X 90 WIDGET-ID 216
     btnClear-2 AT Y 210 X 110 WIDGET-ID 30
     btnPrevQuery-2 AT Y 210 X 130 WIDGET-ID 212
     btnQueries-2 AT Y 210 X 140 WIDGET-ID 190
     btnNextQuery-2 AT Y 210 X 160 WIDGET-ID 214
     btnClipboard-2 AT Y 210 X 170 WIDGET-ID 178
     btnBegins AT Y 120 X 15 WIDGET-ID 74
     btnOK AT Y 210 X 460 WIDGET-ID 132
     btnCancel-2 AT Y 210 X 540 WIDGET-ID 134
     btnBracket AT Y 77 X 15 WIDGET-ID 28
     btnContains AT Y 140 X 15 WIDGET-ID 116
     btnEq AT Y 35 X 15 WIDGET-ID 62
     btnGT AT Y 56 X 45 WIDGET-ID 66
     btnLT AT Y 56 X 15 WIDGET-ID 64
     btnMatches AT Y 161 X 15 WIDGET-ID 114
     btnNE AT Y 35 X 45 WIDGET-ID 68
     btnOr AT Y 98 X 45 WIDGET-ID 24
     btnQt AT Y 77 X 45 WIDGET-ID 72
     btnToday AT Y 182 X 15 WIDGET-ID 122
     rctQueryButtons AT Y 30 X 5 WIDGET-ID 128
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS TOP-ONLY NO-UNDERLINE THREE-D 
         AT X 800 Y 231
         SIZE-PIXELS 625 BY 260
         TITLE "Query Editor"
         DEFAULT-BUTTON btnOK WIDGET-ID 400.

DEFINE FRAME frSettings
     btnSettings AT Y 1 X 1 WIDGET-ID 210
     btnAbout-txt AT Y 187 X 32 WIDGET-ID 208
     btnFavorites AT Y 32 X 1 WIDGET-ID 212
     btnDump-2 AT Y 63 X 1 WIDGET-ID 216
     btnLoad-2 AT Y 94 X 1 WIDGET-ID 220
     btnQueries-3 AT Y 125 X 1 WIDGET-ID 190
     btnChangeLog AT Y 156 X 1 WIDGET-ID 214
     btnAbout AT Y 187 X 1 WIDGET-ID 196
     btnChangeLog-txt AT Y 156 X 32 WIDGET-ID 206
     btnDump-txt AT Y 63 X 32 WIDGET-ID 218
     btnFavorites-txt AT Y 32 X 32 WIDGET-ID 202
     btnLoad-txt AT Y 94 X 32 WIDGET-ID 222
     btnQueries-txt AT Y 125 X 32 WIDGET-ID 204
     btnSettings-txt AT Y 1 X 32 WIDGET-ID 200
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT X 200 Y 252
         SIZE-PIXELS 180 BY 221
         BGCOLOR 15  WIDGET-ID 500.


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
         TITLE              = "DataDigger"
         HEIGHT-P           = 582
         WIDTH-P            = 1508
         MAX-HEIGHT-P       = 2079
         MAX-WIDTH-P        = 1680
         VIRTUAL-HEIGHT-P   = 2079
         VIRTUAL-WIDTH-P    = 1680
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         CONTEXT-HELP-FILE  = "datadigger.chm":U
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}
{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME frSettings:FRAME = FRAME frMain:HANDLE
       FRAME frWhere:FRAME = FRAME frMain:HANDLE.

/* SETTINGS FOR FRAME frMain
   FRAME-NAME                                                           */
/* BROWSE-TAB brFields tgSelAll frMain */
/* BROWSE-TAB brIndexes btnAddFilter2 frMain */
/* BROWSE-TAB brTables btnEditFilter2 frMain */
/* SETTINGS FOR BROWSE brFields IN FRAME frMain
   2                                                                    */
ASSIGN 
       brFields:ALLOW-COLUMN-SEARCHING IN FRAME frMain = TRUE
       brFields:COLUMN-RESIZABLE IN FRAME frMain       = TRUE.

/* SETTINGS FOR BROWSE brIndexes IN FRAME frMain
   3                                                                    */
ASSIGN 
       brIndexes:ALLOW-COLUMN-SEARCHING IN FRAME frMain = TRUE
       brIndexes:COLUMN-RESIZABLE IN FRAME frMain       = TRUE.

ASSIGN 
       brTables:POPUP-MENU IN FRAME frMain             = MENU POPUP-MENU-brTables:HANDLE
       brTables:ALLOW-COLUMN-SEARCHING IN FRAME frMain = TRUE
       brTables:COLUMN-RESIZABLE IN FRAME frMain       = TRUE.

/* SETTINGS FOR BUTTON btnAddFilter IN FRAME frMain
   2                                                                    */
/* SETTINGS FOR BUTTON btnAddFilter2 IN FRAME frMain
   2 3                                                                  */
/* SETTINGS FOR BUTTON btnClearDataFilter IN FRAME frMain
   2                                                                    */
ASSIGN 
       btnClearDataFilter:HIDDEN IN FRAME frMain           = TRUE.

/* SETTINGS FOR BUTTON btnClearFieldFilter IN FRAME frMain
   2                                                                    */
/* SETTINGS FOR BUTTON btnClearIndexFilter IN FRAME frMain
   3                                                                    */
/* SETTINGS FOR BUTTON btnClearTableFilter IN FRAME frMain
   2                                                                    */
/* SETTINGS FOR BUTTON btnDataFilter IN FRAME frMain
   2                                                                    */
ASSIGN 
       btnDataFilter:HIDDEN IN FRAME frMain           = TRUE.

/* SETTINGS FOR BUTTON btnEditFilter IN FRAME frMain
   2                                                                    */
/* SETTINGS FOR BUTTON btnEditFilter2 IN FRAME frMain
   2 3                                                                  */
/* SETTINGS FOR BUTTON btnFieldFilter IN FRAME frMain
   2                                                                    */
/* SETTINGS FOR BUTTON btnIndexFilter IN FRAME frMain
   3                                                                    */
/* SETTINGS FOR BUTTON btnMoveBottom IN FRAME frMain
   2                                                                    */
/* SETTINGS FOR BUTTON btnMoveDown IN FRAME frMain
   2                                                                    */
/* SETTINGS FOR BUTTON btnMoveTop IN FRAME frMain
   2                                                                    */
/* SETTINGS FOR BUTTON btnMoveUp IN FRAME frMain
   2                                                                    */
/* SETTINGS FOR BUTTON btnRemoveFilter IN FRAME frMain
   NO-ENABLE 2                                                          */
/* SETTINGS FOR BUTTON btnRemoveFilter2 IN FRAME frMain
   NO-ENABLE 2 3                                                        */
/* SETTINGS FOR BUTTON btnReset IN FRAME frMain
   2                                                                    */
ASSIGN 
       btnView:POPUP-MENU IN FRAME frMain       = MENU POPUP-MENU-btnView:HANDLE.

/* SETTINGS FOR BUTTON btnViewData IN FRAME frMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFieldsFilter IN FRAME frMain
   3                                                                    */
ASSIGN 
       fiFieldsFilter:PRIVATE-DATA IN FRAME frMain     = 
                "Fields".

/* SETTINGS FOR FILL-IN fiFlagsFilter IN FRAME frMain
   3                                                                    */
ASSIGN 
       fiFlagsFilter:PRIVATE-DATA IN FRAME frMain     = 
                "Flags".

/* SETTINGS FOR FILL-IN fiFormatFilter IN FRAME frMain
   2                                                                    */
ASSIGN 
       fiFormatFilter:PRIVATE-DATA IN FRAME frMain     = 
                "Format".

/* SETTINGS FOR FILL-IN fiIndexInfo IN FRAME frMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiIndexNameFilter IN FRAME frMain
   3                                                                    */
ASSIGN 
       fiIndexNameFilter:PRIVATE-DATA IN FRAME frMain     = 
                "Index Name".

/* SETTINGS FOR FILL-IN fiLabelFilter IN FRAME frMain
   2                                                                    */
ASSIGN 
       fiLabelFilter:PRIVATE-DATA IN FRAME frMain     = 
                "Label".

/* SETTINGS FOR FILL-IN fiLastUsedFilter IN FRAME frMain
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fiNameFilter IN FRAME frMain
   2                                                                    */
ASSIGN 
       fiNameFilter:PRIVATE-DATA IN FRAME frMain     = 
                "Name".

/* SETTINGS FOR FILL-IN fiNumQueriesFilter IN FRAME frMain
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fiOrderFilter IN FRAME frMain
   2                                                                    */
ASSIGN 
       fiOrderFilter:PRIVATE-DATA IN FRAME frMain     = 
                "Order".

/* SETTINGS FOR FILL-IN fiTableDesc IN FRAME frMain
   ALIGN-L                                                              */
ASSIGN 
       fiTableDesc:READ-ONLY IN FRAME frMain        = TRUE.

/* SETTINGS FOR FILL-IN fiTableFilter IN FRAME frMain
   ALIGN-L                                                              */
ASSIGN 
       fiTableFilter:PRIVATE-DATA IN FRAME frMain     = 
                "Table filter".

/* SETTINGS FOR FILL-IN fiTypeFilter IN FRAME frMain
   2                                                                    */
ASSIGN 
       fiTypeFilter:PRIVATE-DATA IN FRAME frMain     = 
                "Type".

/* SETTINGS FOR FILL-IN fiWarning IN FRAME frMain
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fiWarning:HIDDEN IN FRAME frMain           = TRUE.

/* SETTINGS FOR RECTANGLE rcFieldFilter IN FRAME frMain
   NO-ENABLE 2                                                          */
ASSIGN 
       rcFieldFilter:HIDDEN IN FRAME frMain           = TRUE.

/* SETTINGS FOR RECTANGLE rcIndexFilter IN FRAME frMain
   NO-ENABLE 3                                                          */
ASSIGN 
       rcIndexFilter:HIDDEN IN FRAME frMain           = TRUE.

/* SETTINGS FOR RECTANGLE rcTableFilter IN FRAME frMain
   NO-ENABLE                                                            */
ASSIGN 
       rcTableFilter:HIDDEN IN FRAME frMain           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tgDebugMode IN FRAME frMain
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tgDebugMode:HIDDEN IN FRAME frMain           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tgSelAll IN FRAME frMain
   2                                                                    */
/* SETTINGS FOR FRAME frSettings
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME frSettings:HIDDEN           = TRUE
       FRAME frSettings:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME frWhere
                                                                        */
ASSIGN 
       FRAME frWhere:HIDDEN           = TRUE
       FRAME frWhere:MOVABLE          = TRUE.

/* SETTINGS FOR BUTTON btnAnd IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR BUTTON btnBegins IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR BUTTON btnBracket IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR BUTTON btnContains IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR BUTTON btnEq IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR BUTTON btnGT IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR BUTTON btnInsert IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR BUTTON btnLT IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR BUTTON btnMatches IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR BUTTON btnNE IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR BUTTON btnOr IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR BUTTON btnQt IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR BUTTON btnToday IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR BUTTON btnViewData-2 IN FRAME frWhere
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cbAndOr IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR COMBO-BOX cbFields IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR COMBO-BOX cbOperator IN FRAME frWhere
   1                                                                    */
/* SETTINGS FOR FILL-IN ficValue IN FRAME frWhere
   1                                                                    */
ASSIGN 
       ficWhere2:RETURN-INSERTED IN FRAME frWhere  = TRUE.

/* SETTINGS FOR RECTANGLE rctQueryButtons IN FRAME frWhere
   1                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brFields
/* Query rebuild information for BROWSE brFields
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttField.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brFields */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brIndexes
/* Query rebuild information for BROWSE brIndexes
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttIndex.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brIndexes */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brTables
/* Query rebuild information for BROWSE brTables
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttTable.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE brTables */
&ANALYZE-RESUME




/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME FilterTimer ASSIGN
       FRAME           = FRAME frMain:HANDLE
       ROW             = 16.95
       COLUMN          = 145
       HEIGHT          = 1.43
       WIDTH           = 6
       WIDGET-ID       = 218
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME frMain:HANDLE
       ROW             = 16.95
       COLUMN          = 151
       HEIGHT          = 1.43
       WIDTH           = 6
       WIDGET-ID       = 176
       HIDDEN          = yes
       SENSITIVE       = yes.
/* FilterTimer OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: FilterTimer */
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      FilterTimer:MOVE-AFTER(tgDebugMode:HANDLE IN FRAME frMain).
      CtrlFrame:MOVE-AFTER(FilterTimer).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON ALT-F OF C-Win /* DataDigger */
anywhere DO:

  if giCurrentPage = 1 then 
    apply 'entry' to fiNameFilter in frame {&frame-name}.

  return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON ALT-T OF C-Win /* DataDigger */
anywhere DO:
  apply 'entry' to fiTableFilter in frame {&frame-name}.
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON ALT-W OF C-Win /* DataDigger */
anywhere DO:
  apply 'entry' to ficWhere in frame {&frame-name}.
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* DataDigger */
OR ENDKEY OF {&WINDOW-NAME} anywhere 
DO:
  if frame frSettings:visible then
  do:
    apply 'leave' to frame frSettings.
    return no-apply.
  end
    .
  if glRowEditActive 
    and (   focus:parent = ghDataBrowse 
         or focus:parent = brFields:handle in frame {&frame-name} ) then 
  do:
    glRowEditActive = no.
    apply 'leave' to focus.
    focus:screen-value = focus:private-data.
    focus:parent:refresh().
    return no-apply.
  end.

  if gcQueryEditorState = 'visible' then
  do:
    setQueryEditor('Hidden').
    return no-apply.
  end.

  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  {&window-name}:window-state = 2.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* DataDigger */
DO:
  /* This event will close the window and terminate the procedure. */

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* DataDigger */
DO:
  define variable iButtonSpacingX as integer    no-undo.
  define variable iButtonSpacingY as integer    no-undo.

  define buffer ttField for ttField. 

  setWindowFreeze(yes).

  /* Set frame width */
  frame {&frame-name}:width-pixels  = C-Win:width-pixels no-error.
  frame {&frame-name}:height-pixels = C-Win:height-pixels no-error.

  /* Set width of main rectangles */
  rctQuery:width-pixels  = C-Win:width-pixels - 4 no-error. /* 4 */

  rctQuery:X            = 2 no-error.
  rctQuery:Y            = 2 no-error.

  rctData:width-pixels  = rctQuery:width-pixels no-error.
  rctData:height-pixels = C-Win:height-pixels - rctQuery:height-pixels - 52 no-error.
  rctData:X             = rctQuery:X no-error.
  rctData:Y             = rctQuery:y + rctQuery:height-pixels + 4.

  /* Edit buttons */
  rctEdit:X = rctData:x no-error.
  rctEdit:Y = rctData:y + rctData:height-pixels + 5 no-error.

  /* Positioning of buttons "Add" "Save" etc */
  iButtonSpacingX = 5.
  iButtonSpacingY = 5.
  btnAdd:X      = rctEdit:X     + iButtonSpacingX.
  btnAdd:Y      = rctEdit:Y     + iButtonSpacingY.
  btnEdit:X     = btnAdd:X      + btnAdd:width-pixels + iButtonSpacingX.
  btnEdit:Y     = rctEdit:Y     + iButtonSpacingY.
  btnDelete:X   = btnEdit:X     + btnEdit:width-pixels + iButtonSpacingX.
  btnDelete:Y   = rctEdit:Y     + iButtonSpacingY.

  btnView:X     = btnDelete:X   + btnDelete:width-pixels + (iButtonSpacingX * 7).
  btnView:Y     = rctEdit:Y     + iButtonSpacingY.
  btnDump:X     = btnView:X     + btnView:width-pixels + iButtonSpacingX.
  btnDump:Y     = rctEdit:Y     + iButtonSpacingY.
  btnLoad:X     = btnDump:X     + btnDump:width-pixels + iButtonSpacingX.
  btnLoad:Y     = rctEdit:Y     + iButtonSpacingY.

  fiNumResults:x = rctData:x + rctData:width-pixels - fiNumResults:width-pixels - 40.
  fiNumResults:y = rctData:y + rctData:height-pixels - 7.

  fiIndexInfo:x = rctEdit:x + rctEdit:width-pixels + iButtonSpacingX.
  fiIndexInfo:y = rctEdit:y + iButtonSpacingY.
  fiIndexInfo:width-pixels = rctData:width-pixels - fiIndexInfo:x - 40.

  if valid-handle (ghFieldBrowse) then
  do:
    /* Positioning of browse with fields */
    ghFieldBrowse:width-pixels = rctQuery:width-pixels - 282 /* 222 */.

    /* Index browse has same dimensions as field browse 
     * Due to errors on resizing, first 'park' the browse in the upper 
     * left with width 1, then set the proper size attributes.
     */
    brIndexes:x             = 1.
    brIndexes:width-pixels  = 1.
    brIndexes:x             = ghFieldBrowse:x.            
    brIndexes:y             = ghFieldBrowse:y.            
    brIndexes:width-pixels  = ghFieldBrowse:width-pixels. 
    brIndexes:height-pixels = ghFieldBrowse:height-pixels.
    btnAddFilter2:x         = rctQuery:width-pixels - 25.
    btnEditFilter2:x        = rctQuery:width-pixels - 25.
    btnRemoveFilter2:x      = rctQuery:width-pixels - 25.

    /* resize rectangles around the browse */
    rcFieldFilter:x             = ghFieldBrowse:x - 3.
    rcFieldFilter:y             = ghFieldBrowse:y - 3.
    rcFieldFilter:width-pixels  = ghFieldBrowse:width-pixels + 6.
    rcFieldFilter:height-pixels = ghFieldBrowse:height-pixels + 6.
    rcIndexFilter:x             = brIndexes:x - 3.
    rcIndexFilter:y             = brIndexes:y - 3.
    rcIndexFilter:width-pixels  = brIndexes:width-pixels + 6.
    rcIndexFilter:height-pixels = brIndexes:height-pixels + 6.


    /* right-align buttons with field browse */
    btnClipboard:x = (ghFieldBrowse:x + ghFieldBrowse:width-pixels) - btnClipboard:width-pixels.
    btnNextQuery:x = btnClipboard:x - btnNextQuery:width-pixels + 1.
    btnQueries:x   = btnNextQuery:x - btnQueries:width-pixels + 1.
    btnPrevQuery:x = btnQueries:x   - btnPrevQuery:width-pixels + 1.
    btnClear:x     = btnPrevQuery:x - btnClear:width-pixels + 1.
    btnViewData:x  = btnClear:x     - btnViewData:width-pixels + 1.

    /* And align editor to the left of button btnViewData */
    ficWhere:width-pixels = btnViewData:x - ficWhere:x - 1.

    /* Buttons for field moving */
    btnMoveUp:x       = rctQuery:width-pixels - 25.
    btnMoveDown:x     = rctQuery:width-pixels - 25.
    btnReset:x        = rctQuery:width-pixels - 25.
    btnMoveTop:x      = rctQuery:width-pixels - 25.
    btnMoveBottom:x   = rctQuery:width-pixels - 25.
    btnAddFilter:x    = rctQuery:width-pixels - 25.
    btnEditFilter:x   = rctQuery:width-pixels - 25.
    btnRemoveFilter:x = rctQuery:width-pixels - 25.
  end.

  /* Positioning of browse with data */
  if valid-handle (ghDataBrowse) then
  do:
    ghDataBrowse:Y = 1. /* to safely adjust size */
    ghDataBrowse:width-pixels = rctData:width-pixels - 10.
    ghDataBrowse:height-pixels = rctData:height-pixels - 10 - 23. /* Extra space for filters */
    ghDataBrowse:X = rctData:X + 5.
    ghDataBrowse:Y = rctData:Y + 5 + 21. /* Extra space for filters */

    /* Positioning of filters and filterbuttons for data */
    for each ttField:
      if valid-handle(ttField.hFilter) then
        ttField.hFilter:y = ghDataBrowse:y - 23.
    end.
    btnClearDataFilter:y = ghDataBrowse:y - 23.
    btnDataFilter:y = ghDataBrowse:y - 23.

    run dataScrollNotify(input ghDataBrowse).
  end.

  run resizeFilters(input 0). /* tables  */
  run resizeFilters(input 1). /* fields  */
  run resizeFilters(input 2). /* indexes */

  /* Save settings */
  setRegistry("DataDigger", "Window:x", string(c-win:x) ).                             
  setRegistry("DataDigger", "Window:y", string(c-win:y) ).                             
  setRegistry("DataDigger", "Window:height", string(c-win:height-pixels) ).                             
  setRegistry("DataDigger", "Window:width", string(c-win:width-pixels) ).                             

  run showScrollBars(frame {&frame-name}:handle, no, no).
  setWindowFreeze(no).

end. /* window-resized */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME frSettings
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frSettings C-Win
ON LEAVE OF FRAME frSettings
DO:
  /* If the frame is visible and we click again on the 
   * tools-button, we want the frame to disappear
   */
  etime(yes).
  hide frame frSettings.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME frWhere
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frWhere C-Win
ON LEAVE OF FRAME frWhere /* Query Editor */
DO:
  setQueryEditor('Hidden').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brFields
&Scoped-define SELF-NAME brFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brFields C-Win
ON MOUSE-MENU-CLICK OF brFields IN FRAME frMain
DO:
  define variable hEditor    as handle      no-undo.
  define variable hFieldName as handle      no-undo.
  define variable hFieldType as handle      no-undo.
  define variable cField     as character   no-undo. 
  define variable cColumn    as character   no-undo. 
  define variable lOk        as logical     no-undo. 
  define variable iOldPos    as integer     no-undo. 
  define variable iLength    as integer     no-undo. 

  if not brFields:query:get-buffer-handle(1):available then return. 

  /* Select the row we clicked on */
  run selectClickedRow(brFields:handle, output lOk, output cColumn).
  if not lOk then return. 

  hFieldName = brFields:query:get-buffer-handle(1):buffer-field('cFieldName'):handle.
  hFieldType = brFields:query:get-buffer-handle(1):buffer-field('cDataType'):handle.

  if valid-handle(hFieldName) then 
  do:
    /* If CTRL is pressed, do not insert the linked value */
    cField  = hFieldName:buffer-value.

    if lookup("CTRL", GetKeyList() ) <> 0 or getLinkInfo(cField) = "" then 
      cField  = hFieldName:buffer-value.
    else 
      cField = substitute('&1 = &2', cField, quoter(getLinkInfo(cField))).

    iLength = length(cField).

    /* If the query editor is expanded, do actions to that field */
    hEditor = getActiveQueryEditor().

    /* Remember old position for positioning cursor */
    iOldPos = hEditor:cursor-offset.

    /* No text selected */
    if hEditor:selection-text = "" then
    do:
      /* If ficWhere only holds the text <empty> then delete that */
      if hEditor:screen-value = '<empty>' then hEditor:screen-value = ''.
      hEditor:insert-string(cField).
    end.
    else 
    do:
      hEditor:replace-selection-text(cField).
    end.

    apply "entry" to hEditor.
    hEditor:cursor-offset = iOldPos + iLength.
  end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brFields C-Win
ON OFF-HOME OF brFields IN FRAME frMain
DO:
  if not valid-handle(ghLastFilterField) then
    ghLastFilterField = fiNameFilter:handle.

  setFilterFieldColor(ghLastFilterField).
  apply 'entry' to ghLastFilterField. 

  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brFields C-Win
ON RETURN OF brFields IN FRAME frMain
or " "           of ttField.lShow in browse brFields
or value-changed of ttField.lShow in browse brFields
do:
  define buffer ttField for ttField. 
  define variable cField as character no-undo.

  do with frame {&frame-name}:

    find ttField where ttField.cFullName = brFields:get-browse-column(3):screen-value no-error.
    ttField.lShow = not ttField.lShow.
    cField = ttField.cFieldName.

    /* This will include all extents */
    if    ttField.lMetaField = true
      and ttField.iExtent    > 0 then cField = cField + '*'. 

    brFields:get-browse-column(1):checked = ttField.lShow.

    run showField( input cField, input ttField.lShow).
  end.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brFields C-Win
ON ROW-DISPLAY OF brFields IN FRAME frMain
DO:

    define variable hField as handle.
    define variable iField as integer. 

    do iField = 1 to num-entries(gcFieldBrowseColumnHandles):
      hField = handle(entry(iField,gcFieldBrowseColumnHandles)).

      /* Set background color if field is part of primary index */
      hField:bgcolor = (if ttField.lPrimary = true then 8 else ?).

      /* Set color if format is non-default */
      case entry(iField, gcFieldBrowseColumnNames):
        when 'cFormat' then hField:fgcolor = (if ttField.cFormat <> ttField.cFormatOrg then 12 else ?).
        when 'iOrder'  then hField:fgcolor = (if ttField.iOrder  <> ttField.iOrderOrg  then 12 else ?).
      end case.

    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brFields C-Win
ON SCROLL-NOTIFY OF brFields IN FRAME frMain
, brIndexes, brTables
do:

  define variable lp as memptr  no-undo. 
  define variable X  as integer no-undo.
  define variable Y  as integer no-undo.


  set-size( lp ) = 16. 

  run GetCursorPos(input-output lp). 

  /* Show the location of the mouse relative to the frame */
  run ScreenToClient ( input frame {&frame-name}:hwnd
                     , input lp 
                     ).

  x = get-long( lp, 1 ). 
  y = get-long( lp, 5 ). 

  /* Ignore when we clicked on the vertical scrollbar or 
   * above the horizontal to avoid flashing 
   */
  if   self:name = 'brFields' 
    or self:name = 'brIndexes' then
  do:
    if   x > (brFields:x + brFields:width-pixels - 15) 
      or y < (brFields:y + brFields:height-pixels - 15) 
      or y > (brFields:y + brFields:height-pixels) then return.
  end.

  set-size( lp ) = 0. 

  /* scroll-notify detects a mouse action in the scrollbar area of a browse. */
  run resizeFilters(input 0). /* tables  */
  run resizeFilters(input 1). /* fields  */
  run resizeFilters(input 2). /* indexes */

end. /* scroll-notify */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brFields C-Win
ON START-SEARCH OF brFields IN FRAME frMain
DO:

  run reopenFieldBrowse(brFields:current-column:name,?).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brIndexes
&Scoped-define SELF-NAME brIndexes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brIndexes C-Win
ON MOUSE-MENU-CLICK OF brIndexes IN FRAME frMain
DO:
  define variable cFieldList     as character no-undo. 
  define variable cQuery         as character no-undo. 
  define variable hEditor        as handle    no-undo.
  define variable lOk            as logical   no-undo. 
  define variable cColumnClicked as character no-undo. 

  if not brIndexes:query:get-buffer-handle(1):available then return. 

  /* Select the row we clicked on */
  run selectClickedRow(brIndexes:handle, output lOk, output cColumnClicked).
  if not lOk then return. 

  /* Create a query expression from all the fields in the index */
  cFieldList = brIndexes:query:get-buffer-handle(1):buffer-field('cFieldList'):buffer-value.
  cQuery = getQueryFromFields(cFieldList).

  /* If needed, expand the query editor */
  if logical(getRegistry ("DataDigger", "AutoExpandQueryEditor")) <> no then
    setQueryEditor('visible').

  /* Fill in the query on the screen */
  hEditor = getActiveQueryEditor().
  hEditor:screen-value = formatQueryString(cQuery, gcQueryEditorState = 'visible').

  apply "entry" to hEditor.
  hEditor:cursor-offset = length(entry(1,cQuery,'~n')) + 2.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brIndexes C-Win
ON OFF-HOME OF brIndexes IN FRAME frMain
DO:
  if not valid-handle(ghLastIndexFilter) then
    ghLastIndexFilter = fiIndexNameFilter:handle.

  setFilterFieldColor(ghLastIndexFilter).
  apply 'entry' to ghLastIndexFilter. 

  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brIndexes C-Win
ON ROW-DISPLAY OF brIndexes IN FRAME frMain
DO:
  define variable hField as handle.
  define variable iField as integer. 

  do iField = 1 to num-entries(gcIndexBrowseColumnHandles):
    hField = handle(entry(iField,gcIndexBrowseColumnHandles)).

    /* Set color if index is not active */
    hField:fgcolor = (if ttIndex.lIndexActive = false then 12 else ?).
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brIndexes C-Win
ON START-SEARCH OF brIndexes IN FRAME frMain
DO:

  run reopenIndexBrowse(brIndexes:current-column:name,?).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brTables
&Scoped-define SELF-NAME brTables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brTables C-Win
ON OFF-HOME OF brTables IN FRAME frMain
DO:
  setFilterFieldColor(fiTableFilter:handle).
  apply 'entry' to fiTableFilter.

  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brTables C-Win
ON START-SEARCH OF brTables IN FRAME frMain
DO:

  run reopenTableBrowse(brTables:current-column:name,?,?).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brTables C-Win
ON VALUE-CHANGED OF brTables IN FRAME frMain
do:
  if brTables:query:get-buffer-handle(1):available then
  do:
    gcCurrentTable    = brTables:query:get-buffer-handle(1)::cTableName.
    gcCurrentDatabase = brTables:query:get-buffer-handle(1)::cDatabase.

    btnPrevQuery:sensitive = true.
    btnNextQuery:sensitive = true.
    btnQueries:sensitive   = true.
/*     btnLoad:sensitive      = true. */
  end.
  else 
  do:
    gcCurrentTable    = ''.
    gcCurrentDatabase = entry(1, getDatabaseList() ).

    btnPrevQuery:sensitive = false.
    btnNextQuery:sensitive = false.
    btnQueries:sensitive   = false.
/*     btnLoad:sensitive      = false. */
  end.

  /* The OCX will only execute when etime > 500 */
  /* OCX-timer will take care of the actual action */
  glPendingValueChanged = yes.
  giTimeLastTableChange = etime.

end. /* value-changed of brTables */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frSettings
&Scoped-define SELF-NAME btnAbout
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAbout C-Win
ON CHOOSE OF btnAbout IN FRAME frSettings /* Que */
, btnAbout-txt
DO:
  hide frame frSettings.
  run value(getProgramDir() + 'dAbout.w') persistent.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frMain
&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd C-Win
ON CHOOSE OF btnAdd IN FRAME frMain /* Add */
DO:

  message 'Sorry, not (yet) implemented. Perhaps next version :)'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

  /*
  define variable hQuery as handle     no-undo.
  define variable iNumResults as integer    no-undo.

  ghDataBrowse:fetch-selected-row(1) no-error.
  ghDataBrowse:deselect-selected-row(1) no-error.

  hQuery = ghDataBrowse:query.
  iNumResults = hQuery:num-results.
  hQuery:reposition-to-row(iNumResults).
  ghDataBrowse:select-row (ghDataBrowse:NUM-ITERATIONS).

  btnDelete:sensitive = FALSE.  
  btnAdd:sensitive = FALSE.
  */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddFilter C-Win
ON CHOOSE OF btnAddFilter IN FRAME frMain /* <- */
or 'ctrl-cursor-left' of brFields
DO:

  define variable cField as character  no-undo.
  define variable cOldTable as character  no-undo.

  cField = brFields:query:get-buffer-handle(1)::cFieldName.
  cOldTable = getCurrentTable().

  if lookup(cField,gcFieldFilterList) = 0 then 
  do:
    /* Add field to fieldfilterlist */
    gcFieldFilterList = trim(gcFieldFilterList + ',' + cField,',').

    btnRemoveFilter:sensitive = (gcFieldFilterList <> "").

    run getTablesWithField(input gcFieldFilterList, output table ttTable).
    run reopenTableBrowse(?,?,?).
    if cOldTable <> getCurrentTable() then
      apply 'value-changed' to brTables.
    run setWindowTitle.
  end.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddFilter2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddFilter2 C-Win
ON CHOOSE OF btnAddFilter2 IN FRAME frMain /* <- */
or 'ctrl-cursor-left' of brIndexes
DO:
  define buffer ttIndex for ttIndex. 
  define variable cOldTable as character  no-undo.

  cOldTable = getCurrentTable().

  find ttIndex where ttIndex.cIndexName = brIndexes:query:get-buffer-handle(1)::cIndexName no-error.
  if not available ttIndex then return. 

  /* Add field to fieldfilterlist */
  gcFieldFilterList = ttIndex.cFieldList.

  run getTablesWithField(input gcFieldFilterList, output table ttTable).
  run reopenTableBrowse(?,?,?).
  if cOldTable <> getCurrentTable() then
    apply 'value-changed' to brTables.
  run setWindowTitle.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frWhere
&Scoped-define SELF-NAME btnAnd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAnd C-Win
ON CHOOSE OF btnAnd IN FRAME frWhere /* and */
, btnOr, btnEq, btnNe, btnGt, btnLt, btnToday, btnMatches, btnContains, btnBegins
DO:

  /* No text selected */
  if ficWhere2:selection-text = "" then
    ficWhere2:insert-string(substitute(' &1 ', self:label)).
  else 
    ficWhere2:replace-selection-text(substitute(' &1 ', self:label)).

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBracket
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBracket C-Win
ON CHOOSE OF btnBracket IN FRAME frWhere /* () */
DO:

  /* No text selected */
  if ficWhere2:selection-text = "" then
  do:
    ficWhere2:insert-string(substitute(' &1 ', self:label)).
    ficWhere2:cursor-offset = ficWhere2:cursor-offset - 2.
  end.
  else 
    ficWhere2:replace-selection-text(substitute(' ( &1 ) ', ficWhere2:selection-text)).

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel-2 C-Win
ON CHOOSE OF btnCancel-2 IN FRAME frWhere /* Cancel */
DO:
  ficWhere2:screen-value in frame frWhere = ficWhere:screen-value in frame frMain. 
  setQueryEditor('Hidden').
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frSettings
&Scoped-define SELF-NAME btnChangeLog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnChangeLog C-Win
ON CHOOSE OF btnChangeLog IN FRAME frSettings /* ? */
, btnChangeLog-txt
DO:
  hide frame frSettings.

  define variable cHelpfile as character no-undo.
  define variable iHelpId   as integer   no-undo. 

  cHelpfile = getProgramDir() + 'DataDigger.chm'.
  iHelpId = integer( getRegistry('DataDigger:help', 'NewVersion:hlp')) no-error.

  if iHelpId > 0 then
    system-help cHelpfile context iHelpId.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnChangeLog C-Win
ON MOUSE-MENU-CLICK OF btnChangeLog IN FRAME frSettings /* ? */
DO:
  hide frame frSettings.
  os-command no-wait start value(getProgramDir() + '\DataDigger.txt').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frMain
&Scoped-define SELF-NAME btnClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClear C-Win
ON CHOOSE OF btnClear IN FRAME frMain /* C */
or SHIFT-DEL of ficWhere  in frame frMain
or SHIFT-DEL of ficWhere2 in frame frWhere
or 'CHOOSE' of btnClear-2 in frame frWhere
DO:
  define variable hEditor as handle      no-undo.

  hEditor = getActiveQueryEditor().

  hEditor:screen-value = ''.
  hEditor:bgcolor      = ?. /* default */
  hEditor:fgcolor      = ?. /* default */
  hEditor:tooltip      = ''.

  /* Clear query in ini file */
  setRegistry ( substitute('DB:&1'   , getCurrentDatabase() )
              , substitute('&1:query', getCurrentTable() )
              , '' 
              ).  

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearDataFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearDataFilter C-Win
ON CHOOSE OF btnClearDataFilter IN FRAME frMain /* C */
DO:

  run btnClearDataFilterChoose. 

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearFieldFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearFieldFilter C-Win
ON CHOOSE OF btnClearFieldFilter IN FRAME frMain /* C */
DO:

  fiOrderFilter :screen-value = fiOrderFilter :private-data.
  fiNameFilter  :screen-value = fiNameFilter  :private-data.
  fiTypeFilter  :screen-value = fiTypeFilter  :private-data.
  fiFormatFilter:screen-value = fiFormatFilter:private-data.
  fiLabelFilter :screen-value = fiLabelFilter :private-data.

  fiOrderFilter :modified = no.
  fiNameFilter  :modified = no.
  fiTypeFilter  :modified = no.
  fiFormatFilter:modified = no.
  fiLabelFilter :modified = no.

  setFilterFieldColor(fiOrderFilter :handle).
  setFilterFieldColor(fiNameFilter  :handle).
  setFilterFieldColor(fiTypeFilter  :handle).
  setFilterFieldColor(fiFormatFilter:handle).
  setFilterFieldColor(fiLabelFilter :handle).

  apply 'choose' to btnFieldFilter.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearIndexFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearIndexFilter C-Win
ON CHOOSE OF btnClearIndexFilter IN FRAME frMain /* C */
DO:

  fiIndexNameFilter:screen-value = fiIndexNameFilter:private-data. 
  fiFlagsFilter    :screen-value = fiFlagsFilter    :private-data. 
  fiFieldsFilter   :screen-value = fiFieldsFilter   :private-data. 

  setFilterFieldColor(fiIndexNameFilter:handle).
  setFilterFieldColor(fiFlagsFilter    :handle).
  setFilterFieldColor(fiFieldsFilter   :handle).

  apply 'choose' to btnIndexFilter.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearTableFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearTableFilter C-Win
ON CHOOSE OF btnClearTableFilter IN FRAME frMain /* C */
DO:

  fiTableFilter     :screen-value = fiTableFilter     :private-data.
  cbDatabaseFilter  :screen-value = ' '.
  fiNumQueriesFilter:screen-value = ''.
  fiLastUsedFilter  :screen-value = ?.

  fiTableFilter     :modified = no.
  cbDatabaseFilter  :modified = no.
  fiNumQueriesFilter:modified = no.
  fiLastUsedFilter  :modified = no.

  setFilterFieldColor(fiTableFilter     :handle).
  setFilterFieldColor(cbDatabaseFilter  :handle).
  setFilterFieldColor(fiNumQueriesFilter:handle).
  setFilterFieldColor(fiLastUsedFilter  :handle).

  apply 'choose' to btnTableFilter.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClipboard
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClipboard C-Win
ON CHOOSE OF btnClipboard IN FRAME frMain /* Cp */
or 'ctrl-c' of ficWhere  in frame frMain  
or 'ctrl-c' of ficWhere2 in frame frWhere 
or 'CHOOSE' of btnClipboard-2 in frame frWhere
do:
  define variable cQuery  as character no-undo. 
  define variable hEditor as handle    no-undo.

  hEditor = getActiveQueryEditor().

  if length(hEditor:selection-text) > 0 then 
    cQuery = hEditor:selection-text.
  else 
    cQuery = substitute('for each &1.&2 no-lock &3 &4'
                       , getCurrentDatabase()
                       , getCurrentTable()
                       , (if not hEditor:screen-value begins 'where' then 'where' else '')
                       , trim(hEditor:screen-value)
                       ).

/*  clipboard:value = formatQueryString(cQuery, yes). */
  cQuery = ficWhere:tooltip.
  cQuery = formatQueryString(cQuery, yes).
  clipboard:value = getReadableQuery(cQuery).

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDataDigger
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDataDigger C-Win
ON CHOOSE OF btnDataDigger IN FRAME frMain /* DD */
do:
  /* Set the X and Y a little higher so the new window appears cascaded */
  setRegistry("DataDigger", "Window:x", string(c-win:x + 20) ).                             
  setRegistry("DataDigger", "Window:y", string(c-win:y + 20) ).                             

  /* run value(getProgramDir() + 'wDataDigger.w') persistent. */

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDataFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDataFilter C-Win
ON CHOOSE OF btnDataFilter IN FRAME frMain /* Y */
do:
  run reopenDataBrowse('',?).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete C-Win
ON CHOOSE OF btnDelete IN FRAME frMain /* Delete */
DO:

  run btnDeleteChoose.

                              {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
                              {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END. /* choose of btnDelete */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frSettings
&Scoped-define SELF-NAME btnDump-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDump-2 C-Win
ON CHOOSE OF btnDump-2 IN FRAME frSettings /* Dmp */
, btnDump, btnDump-txt
DO:
  hide frame frSettings.
  run btnDumpChoose. 
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frMain
&Scoped-define SELF-NAME btnEdit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEdit C-Win
ON CHOOSE OF btnEdit IN FRAME frMain /* Edit */
do:
  /*------------------------------------------------------------------------
  Name         : Edit
  Description  : Edit selected records
  ---------------------------------------------------------------------- 
  11-01-2011 pti Created
  ----------------------------------------------------------------------*/

  run btnEditChoose.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnEditFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnEditFilter C-Win
ON CHOOSE OF btnEditFilter IN FRAME frMain /* ... */
or 'choose' of btnEditFilter2
or 'ctrl-insert' of brFields
or 'ctrl-insert' of brIndexes
DO:
  define variable cOldTable as character  no-undo.
  define variable cFilter   as character  no-undo.

  cFilter = gcFieldFilterList.
  run value(getProgramDir() + 'dFilter.w')
   ( input-output cFilter ).
  /* If nothing changed, then don't reset anything on the screen */
  if cFilter = gcFieldFilterList then return no-apply.

  gcFieldFilterList = trim(cFilter, ',').
  cOldTable = getCurrentTable().
  btnRemoveFilter:sensitive = (gcFieldFilterList <> "").

  run getTablesWithField(input gcFieldFilterList, output table ttTable).
  run reopenTableBrowse(?,?,?).

  if cOldTable <> getCurrentTable() then
    apply 'value-changed' to brTables.
  run setWindowTitle.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frSettings
&Scoped-define SELF-NAME btnFavorites
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFavorites C-Win
ON CHOOSE OF btnFavorites IN FRAME frSettings /* Fav */
, menu-item m_Manage_Favorites in menu POPUP-MENU-brTables
, btnFavorites-txt
DO:
  hide frame frSettings.
  run btnFavouritesChoose.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frMain
&Scoped-define SELF-NAME btnFieldFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFieldFilter C-Win
ON CHOOSE OF btnFieldFilter IN FRAME frMain /* Y */
or 'return' of fiOrderFilter, fiNameFilter, fiTypeFilter, fiFormatFilter, fiLabelFilter
DO:
  run reopenFieldBrowse(?,?). /* reopen, while maintaining original sort */
  apply 'entry' to brFields.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnHelp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHelp C-Win
ON CHOOSE OF btnHelp IN FRAME frMain /* Help */
or "HELP" of c-win anywhere 
DO:

  run startWinHelp(focus).

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnIndexFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnIndexFilter C-Win
ON CHOOSE OF btnIndexFilter IN FRAME frMain /* Y */
or 'return' of fiIndexNameFilter, fiFlagsFilter, fiFieldsFilter
DO:
  run reopenIndexBrowse(?,?). /* reopen, while maintaining original sort */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frWhere
&Scoped-define SELF-NAME btnInsert
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnInsert C-Win
ON CHOOSE OF btnInsert IN FRAME frWhere /* + */
or "return" of /* cbAndOr, cbFields, cbOperator, */ ficValue
DO:
  define buffer ttField for ttField. 
  define variable cField as character no-undo.

  find ttField where ttField.cFullName = cbFields:screen-value no-error.
  if not available ttField then return. 
  cField = ttField.cFullName.

  if cField = 'RECID' or cField = 'ROWID' 
    then cField = substitute('&1(&2)', cField, getCurrentTable() ).

  ficWhere2:insert-string(left-trim(substitute('&1 &2 &3 &4&5'
                                        , (if cbAndOr:screen-value = ? then '' else cbAndOr:screen-value)
                                        , cField
                                        , cbOperator:screen-value
                                        , if ttField.cDataType = 'character' then quoter(ficValue:screen-value) else ficValue:screen-value
                                        , chr(13)
                                        )
                              )
                         ).

  apply "entry" to cbAndOr.
  return no-apply.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frSettings
&Scoped-define SELF-NAME btnLoad-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLoad-2 C-Win
ON CHOOSE OF btnLoad-2 IN FRAME frSettings /* Load */
, btnLoad, btnLoad-txt
DO:
  hide frame frSettings.
  run loadData.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frMain
&Scoped-define SELF-NAME btnMoveBottom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveBottom C-Win
ON CHOOSE OF btnMoveBottom IN FRAME frMain /* Btm */
or 'ctrl-end' of brFields
DO:

  run moveField('bottom').

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveDown C-Win
ON CHOOSE OF btnMoveDown IN FRAME frMain /* Dn */
or 'ctrl-cursor-down' of brFields
DO:

  run moveField('down').

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveTop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveTop C-Win
ON CHOOSE OF btnMoveTop IN FRAME frMain /* Top */
or 'ctrl-home' of brFields
DO:

  run moveField('top').

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveUp C-Win
ON CHOOSE OF btnMoveUp IN FRAME frMain /* Up */
or 'ctrl-cursor-up' of brFields
DO:

  run moveField('up').

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNextQuery
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNextQuery C-Win
ON CHOOSE OF btnNextQuery IN FRAME frMain /* > */
or 'page-up' of ficWhere       in frame frMain 
or 'choose'  of btnNextQuery-2 in frame frWhere
do:
  setQuery(-1).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frWhere
&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK C-Win
ON CHOOSE OF btnOK IN FRAME frWhere /* OK */
DO:
  setQueryEditor('Hidden').
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frMain
&Scoped-define SELF-NAME btnPrevQuery
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrevQuery C-Win
ON CHOOSE OF btnPrevQuery IN FRAME frMain /* < */
OR 'page-down' of ficWhere       in frame frMain 
or 'choose'    of btnPrevQuery-2 in frame frWhere
do:
  setQuery(+1).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frWhere
&Scoped-define SELF-NAME btnQt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnQt C-Win
ON CHOOSE OF btnQt IN FRAME frWhere /* "" */
DO:

  /* No text selected */
  if ficWhere2:selection-text = "" then
  do:
    ficWhere2:insert-string(substitute(' &1 ', self:label)).
    ficWhere2:cursor-offset = ficWhere2:cursor-offset - 2.
  end.
  else 
    ficWhere2:replace-selection-text(substitute('"&1"', ficWhere2:selection-text)).

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frMain
&Scoped-define SELF-NAME btnQueries
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnQueries C-Win
ON CHOOSE OF btnQueries IN FRAME frMain /* Q */
or 'ALT-Q'    of c-win 
or 'CTRL-INS' of ficWhere     in frame frMain
or 'CTRL-INS' of ficWhere2    in frame frWhere 
or 'CHOOSE'   of btnQueries-2 in frame frWhere 
DO:
  define variable iQuery  as integer no-undo. 
  define variable hEditor as handle  no-undo.

  hEditor = getActiveQueryEditor().

  run value(getProgramDir() + 'dQueries.w')
    ( input getCurrentDatabase()
    , input getCurrentTable()
    , input hEditor:screen-value
    , output iQuery
    ).

  if iQuery = ? then 
    return.
  else 
  do:
    /* Queries might be changed, so reload them */
    run collectQueryInfo
      ( input getCurrentDatabase()
      , input getCurrentTable() 
      ).
    giQueryPointer = iQuery.
    setQuery(0).
  end.


    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frSettings
&Scoped-define SELF-NAME btnQueries-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnQueries-3 C-Win
ON CHOOSE OF btnQueries-3 IN FRAME frSettings /* Q */
or 'ALT-Q'    of c-win 
or 'CTRL-INS' of ficWhere       in frame frMain
or 'CTRL-INS' of ficWhere2      in frame frWhere 
or 'CHOOSE'   of btnQueries-2   in frame frWhere 
or 'CHOOSE'   of btnQueries-txt in frame frSettings
DO:
  hide frame frSettings.
  run btnQueriesChoose.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frMain
&Scoped-define SELF-NAME btnQueryTester
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnQueryTester C-Win
ON CHOOSE OF btnQueryTester IN FRAME frMain /* QT */
DO:

  define variable cQueryTesterPath as character no-undo.

  cQueryTesterPath = getRegistry
    ( 'QueryTester'
    , 'path' 
    ).

  file-info:file-name = search(cQueryTesterPath).
  if file-info:full-pathname <> ? then
    run value(cQueryTesterPath) persistent.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRemoveFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemoveFilter C-Win
ON CHOOSE OF btnRemoveFilter IN FRAME frMain /* -> */
or 'ctrl-cursor-right' of brFields
DO:
  define variable cOldTable as character  no-undo.

  /* Remove field from fieldfilterlist */
  if num-entries(gcFieldFilterList) > 0 then 
  do:
    entry(num-entries(gcFieldFilterList),gcFieldFilterList) = ''.
    gcFieldFilterList = trim(gcFieldFilterList, ',').
    cOldTable = getCurrentTable().

    btnRemoveFilter:sensitive = (gcFieldFilterList <> "").

    run getTablesWithField(input gcFieldFilterList, output table ttTable).
    run reopenTableBrowse(?,?,?).
    if cOldTable <> getCurrentTable() then
      apply 'value-changed' to brTables.
    run setWindowTitle.
  end.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRemoveFilter2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemoveFilter2 C-Win
ON CHOOSE OF btnRemoveFilter2 IN FRAME frMain /* -> */
or 'ctrl-cursor-right' of brIndexes
DO:
  define variable cOldTable as character  no-undo.

  /* Remove field from fieldfilterlist */
  do:
    gcFieldFilterList = "".
    cOldTable = getCurrentTable().
    btnRemoveFilter2:sensitive = (gcFieldFilterList <> "").

    run getTablesWithField(input gcFieldFilterList, output table ttTable).
    run reopenTableBrowse(?,?,?).
    if cOldTable <> getCurrentTable() then
      apply 'value-changed' to brTables.
    run setWindowTitle.
  end.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReset C-Win
ON CHOOSE OF btnReset IN FRAME frMain /* R */
DO:
  define buffer ttField for ttField.

  /* All fields */
  for each ttField:
    ttField.iOrder = ttField.iOrderOrg.
  end.

  /* remove order from ini file */
  setRegistry( substitute('DB:&1',getCurrentDatabase())
             , substitute('&1:fieldOrder', getCurrentTable() )
             , ?
             ).

  run reopenFieldBrowse('iOrder', yes).

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frSettings
&Scoped-define SELF-NAME btnSettings
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSettings C-Win
ON CHOOSE OF btnSettings IN FRAME frSettings /* INI */
, btnSettings-txt 
DO:
  hide frame frSettings.
  run btnSettingsChoose.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSettings C-Win
ON MOUSE-MENU-CLICK OF btnSettings IN FRAME frSettings /* INI */
DO:
  define variable cEnvironment as character   no-undo.

  hide frame frSettings.

  /* Load or create personalized ini file */
  cEnvironment = substitute('&1DataDigger-&2.ini'
                           , getTempDir()
                           , getUserName() 
                           ).

  /* Start default editor for ini file */
  os-command no-wait start value( cEnvironment ).


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frMain
&Scoped-define SELF-NAME btnTabFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTabFields C-Win
ON CHOOSE OF btnTabFields IN FRAME frMain /* Fld */
or 'ctrl-1' of frame {&frame-name} anywhere
DO:
  run setPage(1).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTabIndexes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTabIndexes C-Win
ON CHOOSE OF btnTabIndexes IN FRAME frMain /* Idx */
or 'ctrl-2' of frame {&frame-name} anywhere
DO:
  run setPage(2).
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTabIndexes C-Win
ON MOUSE-MENU-CLICK OF btnTabIndexes IN FRAME frMain /* Idx */
DO:
  define variable dRow           as decimal     no-undo.
  define variable iRow           as integer     no-undo.
  define variable cFieldList     as character no-undo. 
  define variable cQuery         as character no-undo. 

  /* Find the primary index */
  find first ttIndex where ttIndex.cIndexFlags matches '*P*' no-error.
  if not available ttIndex then return. 

  /* Create a query expression from all the fields in the index */
  cQuery = getQueryFromFields(ttIndex.cFieldList).

  /* If needed, expand the query editor */
  if logical(getRegistry ("DataDigger", "AutoExpandQueryEditor")) <> no then
    setQueryEditor('visible').

  /* Fill in the query on the screen */
  ficWhere:screen-value = formatQueryString(cQuery, gcQueryEditorState = 'visible').

  apply "entry" to ficWhere.
  ficWhere:cursor-offset = length(entry(1,cQuery,'~n')) + 2.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTableFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTableFilter C-Win
ON CHOOSE OF btnTableFilter IN FRAME frMain /* Y */
or 'return' of fiTableFilter, cbDatabaseFilter, fiNumQueriesFilter, fiLastUsedFilter
or 'value-changed' of menu-item m_Show_hidden_tables in menu POPUP-MENU-brTables
DO:
  glPendingValueChanged = yes.
  run reopenTableBrowse(?,?,?).
  apply 'value-changed' to brTables.
  apply 'entry' to brTables.
  return no-apply.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTools
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTools C-Win
ON CHOOSE OF btnTools IN FRAME frMain /* Tools */
DO:
  DEFINE VARIABLE ReturnValue AS INTEGER NO-UNDO.
  &SCOP BM_SETSTATE 243

  if etime < 100 then return no-apply.

  if frame frSettings:visible then
  do:
    hide frame frSettings.
    return no-apply.
  end.

  /* Set button to 'pushed' state */
  frame frSettings:x = 35.
  frame frSettings:y = 35.
  view frame frSettings.

  apply 'entry' to btnSettings in frame frSettings.
  return no-apply.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnView C-Win
ON CHOOSE OF btnView IN FRAME frMain /* View */
DO:

  run btnViewChoose.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnViewData
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnViewData C-Win
ON CHOOSE OF btnViewData IN FRAME frMain /* -> */
or 'ctrl-j' of cbAndOr, cbFields, cbOperator, ficValue, ficWhere, fiTableFilter, brTables,
               fiOrderFilter, fiNameFilter, fiTypeFilter, fiFormatFilter, fiLabelFilter, brFields
or mouse-select-dblclick, return of brTables
or 'ctrl-j' of ficWhere2 in frame frWhere
or 'CTRL-J' of ttField.cFormat
or 'CHOOSE' of btnViewData-2 in frame frWhere
do:
  /* Only proceed if the button is sensitive */
  if not btnViewData:sensitive then 
    return no-apply.

  setWindowFreeze(yes).

  /* If needed, close the query editor  */
  setQueryEditor('hidden').

  /* If there is a pending change, first refresh the table context */
  if glPendingValueChanged then 
  do:
    run setTableContext(input getCurrentTable()).
    glPendingValueChanged = no.
  end.

  run reopenDataBrowse('',?).

  setWindowFreeze(no).

  if valid-handle(ghDataBrowse) then
    apply 'entry' to ghDataBrowse. 
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnWhere
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnWhere C-Win
ON CHOOSE OF btnWhere IN FRAME frMain /* Where */
DO:
  case gcQueryEditorState:
    when 'visible' then 
    do: 
        setQueryEditor('hidden').
        apply 'entry' to ficWhere in frame frMain.
    end.

    when 'hidden'  then 
    do:
      setQueryEditor('visible').
      apply 'entry' to ficWhere2 in frame frWhere.
    end.

  end case.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frWhere
&Scoped-define SELF-NAME cbAndOr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbAndOr C-Win
ON RETURN OF cbAndOr IN FRAME frWhere /* Where */
DO:
  apply 'entry' to cbFields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbFields C-Win
ON RETURN OF cbFields IN FRAME frWhere
DO:
  apply 'entry' to cbOperator.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbOperator
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbOperator C-Win
ON RETURN OF cbOperator IN FRAME frWhere
DO:
  apply 'entry' to ficValue.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frMain
&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  if glPendingValueChanged 
    and (etime - giTimeLastTableChange) > chCtrlFrame:PSTimer:interval then 
  do:
    setWindowFreeze(yes).

    run setTableContext(input getCurrentTable() ).

    setWindowFreeze(no).

    glPendingValueChanged = no.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frWhere
&Scoped-define SELF-NAME ficValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ficValue C-Win
ON ENTRY OF ficValue IN FRAME frWhere
DO:
  if self:screen-value = "" then 
    self:screen-value = getLinkInfo(cbFields:screen-value). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frMain
&Scoped-define SELF-NAME ficWhere
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ficWhere C-Win
ON ALT-CURSOR-DOWN OF ficWhere IN FRAME frMain
DO:
  setQueryEditor('visible').
  apply 'entry' to ficWhere2 in frame frWhere.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ficWhere C-Win
ON RETURN OF ficWhere IN FRAME frMain
DO:
  /* If the editor is small, interpret an ENTER as CTRL-ENTER */
  if gcQueryEditorState = 'hidden' then 
  do:
    apply 'choose' to btnViewData.
    return no-apply.
  end. 
  else
    self:insert-string ( '~n' ) .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frWhere
&Scoped-define SELF-NAME ficWhere2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ficWhere2 C-Win
ON ALT-CURSOR-UP OF ficWhere2 IN FRAME frWhere
DO:
  setQueryEditor('hidden').
  apply 'entry' to ficWhere in frame frMain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frMain
&Scoped-define SELF-NAME fiIndexNameFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiIndexNameFilter C-Win
ON SHIFT-DEL OF fiIndexNameFilter IN FRAME frMain
, fiFlagsFilter, fiFieldsFilter
DO:
  apply 'choose' to btnClearIndexFilter.
  self:screen-value = ''.
  apply 'value-changed' to self.
  apply 'entry' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLastUsedFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLastUsedFilter C-Win
ON CURSOR-DOWN OF fiLastUsedFilter IN FRAME frMain
DO:
  setFilterFieldColor(self:handle).
  apply 'entry' to brTables.

  /* If, by applying a filter, the table has disappeared, 
   * select the first in the list 
   */
/*   if slTableName:screen-value = ? then                          */
/*   do:                                                           */
/*     slTableName:screen-value = entry(1,slTableName:list-items). */
/*     apply 'value-changed' to slTableName.                       */
/*   end.                                                          */

  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FilterTimer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FilterTimer C-Win OCX.Tick
PROCEDURE FilterTimer.FilterTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  if glPendingFilterChange
    and (etime - giTimeLastFilterChange) > chFilterTimer:FilterTimer:interval then 
  do:

    glPendingFilterChange = false.

    case gcLastFilterChangeBrowse:

      when 'brTables' then do:
        glPendingValueChanged = yes. /* to refresh field list */
        run reopenTableBrowse(?,?,?).
        apply 'value-changed' to brTables in frame {&frame-name}.
      end.

      when 'brFields' then do:
        run reopenFieldBrowse(?,?). /* reopen, while maintaining original sort */
      end.

      when 'brIndexes' then do:
        run reopenIndexBrowse(?,?). /* reopen, while maintaining original sort */
      end.
    end case. 

  end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiNumQueriesFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiNumQueriesFilter C-Win
ON CURSOR-DOWN OF fiNumQueriesFilter IN FRAME frMain
DO:
  setFilterFieldColor(self:handle).
  apply 'entry' to brTables.

  /* If, by applying a filter, the table has disappeared, 
   * select the first in the list 
   */
/*   if slTableName:screen-value = ? then                          */
/*   do:                                                           */
/*     slTableName:screen-value = entry(1,slTableName:list-items). */
/*     apply 'value-changed' to slTableName.                       */
/*   end.                                                          */

  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiNumresults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiNumresults C-Win
ON MOUSE-SELECT-DBLCLICK OF fiNumresults IN FRAME frMain
DO:
  define variable hQuery  as handle      no-undo.
  define variable hBuffer as handle      no-undo.
  define variable cQuery  as character   no-undo.
  define variable cTable  as character   no-undo.
  define variable iStartTime as integer     no-undo.

  if not valid-handle(ghDataBrowse) then return.

  session:set-wait-state('general'). 
  iStartTime = etime.

  /* Change query to a PRESELECT query to get number of rows */
  cQuery = ghDataBrowse:query:prepare-string.
  entry(1,cQuery,' ') = 'preselect'.

  /* Create buffer for the table we selected in the data browse */
  cTable = ghDataBrowse:query:get-buffer-handle(1):dbname + '.' + ghDataBrowse:query:get-buffer-handle(1):name.

  create query hQuery.
  create buffer hBuffer for table ghDataBrowse:query:get-buffer-handle(1):dbname + '.' + ghDataBrowse:query:get-buffer-handle(1):name no-error.

  hQuery:set-buffers(hBuffer).
  hQuery:query-prepare(cQuery).
  hQuery:query-open.

  session:set-wait-state('').

  /* Show nr of records */
  setNumRecords(hQuery:num-results, yes, etime - iStartTime).

  hQuery:query-close.

  delete object hQuery.
  delete object hBuffer.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiOrderFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiOrderFilter C-Win
ON ANY-PRINTABLE OF fiOrderFilter IN FRAME frMain
, fiNameFilter, fiTypeFilter, fiFormatFilter, fiLabelFilter
, fiTableFilter, fiLastUsedFilter
, fiIndexNameFilter, fiFlagsFilter, fiFieldsFilter
do:

  setFilterFieldColor(self:handle).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiOrderFilter C-Win
ON CURSOR-DOWN OF fiOrderFilter IN FRAME frMain
, fiNameFilter, fiTypeFilter, fiFormatFilter, fiLabelFilter
, fiIndexNameFilter, fiFlagsFilter, fiFieldsFilter
DO:

  apply 'leave' to self.

  case giCurrentPage:
    when 1 then 
    do:
      /* Save name of field we escaped from */
      assign ghLastFilterField = self.
      apply 'entry' to ttField.cFieldName in browse brFields.
    end.

    when 2 then
    do:
      /* Save name of field we escaped from */
      assign ghLastIndexFilter = self.
      apply 'entry' to ttIndex.cIndexName in browse brIndexes.
    end.
  end case.

  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiOrderFilter C-Win
ON ENTRY OF fiOrderFilter IN FRAME frMain
, fiNameFilter, fiTypeFilter, fiFormatFilter, fiLabelFilter
, fiTableFilter, fiLastUsedFilter
, fiIndexNameFilter, fiFlagsFilter, fiFieldsFilter
do:
  /* If you enter the field and you have not put in a filter, 
   * clear out the field so you can type something yourself
   */
  if self:screen-value = self:private-data then
    self:screen-value = ''.

  setFilterFieldColor(self:handle).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiOrderFilter C-Win
ON LEAVE OF fiOrderFilter IN FRAME frMain
, fiNameFilter, fiTypeFilter, fiFormatFilter, fiLabelFilter
, fiTableFilter, fiLastUsedFilter
, fiIndexNameFilter, fiFlagsFilter, fiFieldsFilter
do:
  if self:screen-value = '' then self:screen-value = self:private-data.

  setFilterFieldColor(self:handle).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiOrderFilter C-Win
ON SHIFT-DEL OF fiOrderFilter IN FRAME frMain
, fiNameFilter, fiTypeFilter, fiFormatFilter, fiLabelFilter
DO:
  apply 'choose' to btnClearFieldFilter.
  self:screen-value = ''.
  apply 'value-changed' to self.
  apply 'entry' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTableFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTableFilter C-Win
ON CURSOR-DOWN OF fiTableFilter IN FRAME frMain
DO:
  setFilterFieldColor(self:handle).
  apply 'entry' to brTables.
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTableFilter C-Win
ON SHIFT-DEL OF fiTableFilter IN FRAME frMain
, cbDatabaseFilter, cbDatabaseFilter, fiLastUsedFilter
DO:
  apply 'choose' to btnClearIndexFilter.
  self:screen-value = ''.
  apply 'value-changed' to self.
  apply 'entry' to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTableFilter C-Win
ON VALUE-CHANGED OF fiTableFilter IN FRAME frMain
, cbDatabaseFilter, fiLastUsedFilter
, fiOrderFilter, fiNameFilter, fiTypeFilter, fiFormatFilter, fiLabelFilter
, fiIndexNameFilter, fiFlagsFilter, fiFieldsFilter
DO:
  define variable cSetting as character   no-undo.

  giTimeLastFilterChange = etime.
  glPendingFilterChange = true.

  /* Save last used database */
  if self:name = 'cbDatabaseFilter' then 
  do:
    cSetting = cbDatabaseFilter:screen-value in frame {&frame-name}.
    if cSetting = ? then cSetting = '<empty>'.
    setRegistry("DataDigger", "Database", cSetting ).
  end.

  if lookup(self:name,'fiTableFilter,cbDatabaseFilter') > 0 then
    gcLastFilterChangeBrowse = 'brTables'.
  else
  if lookup(self:name,'fiOrderFilter,fiNameFilter,fiTypeFilter,fiFormatFilter,fiLabelFilter') > 0 then
    gcLastFilterChangeBrowse = 'brFields'.
  else
  if lookup(self:name,'fiIndexNameFilter,fiFlagsFilter,fiFieldsFilter') > 0 then
    gcLastFilterChangeBrowse = 'brIndexes'.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Disconnect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Disconnect C-Win
ON CHOOSE OF MENU-ITEM m_Disconnect /* Disconnect */
or '-',delete-character of cbDatabaseFilter
DO:
  DEFINE VARIABLE cDatabases  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cCurrentDb  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE lDisconnect AS LOGICAL     NO-UNDO.
  define variable hBuffer as handle no-undo.

  do with frame {&frame-name}:
    hBuffer = brTables:query:get-buffer-handle(1).
    if hBuffer:available then 
      cCurrentDb = hBuffer::cDatabase.
    else 
      return.
  end.

  /* Cannot disconnect 'all' */
  if cCurrentDb = '' or cCurrentDb = ? then return. 

  /* Cannot disconnect last database */
/*   if num-dbs = 1 then                          */
/*   do:                                          */
/*     run showHelp('NeedDatabase', cCurrentDb).  */
/*     return.                                    */
/*   end.                                         */

  /* Confirm by user */
  run showHelp('Disconnect', cCurrentDb).
  if getRegistry('DataDigger:help', 'Disconnect:answer') <> '1' then return.

  disconnect value(cCurrentDb).

  /* Remove all tables of this db from the 'tables' table */
  for each ttTable where ttTable.cDatabase = cCurrentDb:
    delete ttTable. 
  end.

  /* Get all connected databases */
  cDatabases = getDatabaseList().
  cbDatabaseFilter:list-items = ',' + cDatabases.
  /* run setConnectionMenu. */

  /* Select a new database as the current db */
  if cbDatabaseFilter:screen-value = cCurrentDb then
    cbDatabaseFilter:screen-value = ''.

  apply 'value-changed' to brTables.  /* this sets the gcCurrentDatabase */
  apply 'choose' to btnTableFilter. 

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Quick_Connect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Quick_Connect C-Win
ON CHOOSE OF MENU-ITEM m_Quick_Connect /* Quick Connect */
or '+', insert-mode of cbDatabaseFilter 
DO:
  DEFINE VARIABLE cPhysicalName AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cLogicalName  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cTypes        AS CHARACTER   no-undo initial 'PROGRESS'.
  DEFINE VARIABLE cDatabases    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iNumDbs       AS INTEGER     NO-UNDO.

  iNumDbs = num-dbs.

  run adecomm\_dbconn.p ( input-output cPhysicalName
                        , input-output cLogicalName
                        , input-output cTypes
                        ).

  if num-dbs = iNumDbs then 
    return. /* nothing connected */

  /* Rebuild context menu */
  /* run setConnectionMenu. */

  /* Get list of all tables of all databases */
  run getTables(output table ttTable).

  /* Get all connected databases */
  cDatabases = getDatabaseList().
  cbDatabaseFilter:list-items = ',' + cDatabases.
  cbDatabaseFilter:screen-value = cLogicalName.
  apply 'value-changed' to cbDatabaseFilter. 
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Show_hidden_tables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Show_hidden_tables C-Win
ON VALUE-CHANGED OF MENU-ITEM m_Show_hidden_tables /* Show hidden tables */
DO:
  define variable cSetting as character   no-undo.

  cSetting = string(menu-item m_Show_hidden_tables:checked in menu POPUP-MENU-brTables).
  setRegistry('DataDigger', 'ShowHiddenTables', cSetting ).

  gcLastFilterChangeBrowse = 'brTables'.
  glPendingFilterChange = true.
  giTimeLastFilterChange = 0.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_View_as_Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_View_as_Excel C-Win
ON CHOOSE OF MENU-ITEM m_View_as_Excel /* View as Excel */
DO:
  run setViewType('XLS').
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_View_as_HTML
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_View_as_HTML C-Win
ON CHOOSE OF MENU-ITEM m_View_as_HTML /* View as HTML */
DO:
  run setViewType('HTML').
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_View_as_text
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_View_as_text C-Win
ON CHOOSE OF MENU-ITEM m_View_as_text /* View as TEXT */
DO:
  run setViewType('TXT').
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgDebugMode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgDebugMode C-Win
ON VALUE-CHANGED OF tgDebugMode IN FRAME frMain /* debugmode */
DO:

  glDebugMode                       = self:checked.
  chFilterTimer:FilterTimer:enabled = not self:checked.
  chCtrlFrame:PSTimer:enabled       = not self:checked.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgSelAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgSelAll C-Win
ON VALUE-CHANGED OF tgSelAll IN FRAME frMain
DO:

  run showField(input '*', input self:checked).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brFields
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


session:debug-alert = yes.

/* ***************************  Main Block  *************************** */
publish "timerCommand" ("start", "startDiggerLib").
run startDiggerLib.
publish "timerCommand" ("stop", "startDiggerLib").

subscribe to 'DataDiggerClose' anywhere.

/* Avoid drawing */
{&window-name}:y                 = -1000. /* out of sight */
{&window-name}:visible           = yes. /* otherwise lockwindow complains */
{&window-name}:max-width-pixels  = ?.
{&window-name}:max-height-pixels = ?.

setWindowFreeze(yes).

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
do:
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.
  define variable cSetting as character no-undo. 

  /* Save settings */
  setRegistry("DataDigger", "Window:x", string(c-win:x) ).                             
  setRegistry("DataDigger", "Window:y", string(c-win:y) ).                             
  setRegistry("DataDigger", "Window:height", string(c-win:height-pixels) ).                             
  setRegistry("DataDigger", "Window:width", string(c-win:width-pixels) ).                             

  cSetting = cbDatabaseFilter:screen-value in frame {&frame-name}.
  if cSetting = ? then cSetting = '<empty>'.
  setRegistry("DataDigger", "Database", cSetting ).

  run disable_UI.

  /* Notify launcher that the window closes */
  publish 'DataDigger'(-1).
end. /* CLOSE OF THIS-PROCEDURE  */


on 'menu-drop' of menu POPUP-MENU-brTables
do:
  run setConnectionMenu.
end.


on entry of ttField.cFormat in browse brFields
do:
  do with frame {&frame-name}:
    define variable cOrgValue as character no-undo. 

    apply "ENTRY" to self.    
    cOrgValue = brFields:query:get-buffer-handle(1):buffer-field('cFormatOrg'):buffer-value.
    glRowEditActive = yes.
    self:private-data = self:screen-value.

    if cOrgValue <> self:screen-value then
    do:
      fiWarning:x            = 300.
      fiWarning:y            = self:y + brFields:y - 2.
      fiWarning:visible      = yes.
      fiWarning:screen-value = substitute('Original format: &1', cOrgValue).
      fiWarning:width        = length(fiWarning:screen-value) + 1.
      fiWarning:x            = self:x - fiWarning:width-pixels + brFields:x - 10.
    end.
    return no-apply.
  end. 
end. /* on entry of ttField.cFormat */

on leave of ttField.cFormat in browse brFields
do:
  do with frame {&frame-name}:
    define variable cOrgValue as character no-undo. 
    define variable cTable    as character no-undo. 
    define variable cField    as character no-undo. 

    fiWarning:visible = no.
    fiWarning:x = 1.

    cTable    = getCurrentTable().
    cField    = brFields:query:get-buffer-handle(1):buffer-field('cFieldName'):buffer-value.
    cOrgValue = brFields:query:get-buffer-handle(1):buffer-field('cFormatOrg'):buffer-value.
    glRowEditActive = no.

    if self:screen-value = '' then self:screen-value = cOrgValue.
    self:fgcolor = (if self:screen-value <> cOrgValue then 12 else ?).

    /* Save changed format. If it is blank, it will be deleted from registry */
    setRegistry( substitute('DB:&1',getCurrentDatabase())
               , substitute('&1.&2:format',cTable,cField)
               , if self:screen-value <> cOrgValue then self:screen-value else ?
               ).
  end. 
end. /* on leave of ttField.cFormat */


ON CTRL-TAB OF C-Win anywhere /* DataDigger */
DO:
  case giCurrentPage:
    when 1 then run setPage(2).
    when 2 then run setPage(1).
  end case.
END. /* CTRL-TAB OF C-Win anywhere */

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  /* Notify launcher that the window started */
  publish 'DataDigger'(+1).

  run enable_UI.
  run initializeObjects.
  setWindowFreeze(no).

  publish "timerCommand" ("stop", "Startup").
  run welcome.

  apply 'entry' to fiTableFilter.

    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p on 04.18.2017 @ 11:36:33 am */
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  if not this-procedure:persistent then 
    wait-for close of this-procedure.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnClearDataFilterChoose C-Win 
PROCEDURE btnClearDataFilterChoose :
/*------------------------------------------------------------------------
  Name         : btnClearDataFilterChoose
  Description  : Clear filters and reopen data browse
  ---------------------------------------------------------------------- 
  04-02-2011 pti Created
  ----------------------------------------------------------------------*/

  define buffer ttField for ttField. 

  for each ttField:
    if valid-handle(ttField.hFilter) then
    do:
      assign
        ttField.hFilter:modified     = no
        ttField.hFilter:screen-value = ttField.hFilter:private-data.

      run filterFieldValueChanged(ttField.hFilter).
      setFilterFieldColor(ttField.hFilter).
    end.
  end.

  run reopenDataBrowse('',?).

end procedure. /* btnClearDataFilterChoose */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnDeleteChoose C-Win 
PROCEDURE btnDeleteChoose :
/*------------------------------------------------------------------------
  Name         : btnDeleteChoose 
  Description  : Delete selected records
  ---------------------------------------------------------------------- 
  18-03-2009 pti Created
  ----------------------------------------------------------------------*/

  define variable iCount    as integer     no-undo.
  define variable hBuffer   as handle      no-undo.
  define variable lContinue as logical     no-undo.

  run showHelp('ConfirmDelete', string(ghDataBrowse:num-selected-rows)).
  if getRegistry('DataDigger:help', 'ConfirmDelete:answer') <> '1' then return.

  /* Dump the record as a backup */
  run dumpRecord( input 'Delete'
                , input ghDataBrowse
                , output lContinue
                ).
  if not lContinue then return. 

  /* Do the delete */
  DO iCount = 1 TO ghDataBrowse:NUM-SELECTED-ROWS:
    ghDataBrowse:FETCH-SELECTED-ROW(iCount).

    DO TRANSACTION:
      ghDataBrowse:QUERY:GET-CURRENT(EXCLUSIVE-LOCK).
      hBuffer = ghDataBrowse:QUERY:GET-BUFFER-HANDLE().
      hBuffer:DISABLE-LOAD-TRIGGERS(yes).
      hBuffer:DISABLE-DUMP-TRIGGERS( ).

      hBuffer:BUFFER-DELETE().
    END.

    ghDataBrowse:QUERY:GET-NEXT(NO-LOCK).
  END.

  ghDataBrowse:REFRESH().

END PROCEDURE. /* btnDeleteChoose */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnDumpChoose C-Win 
PROCEDURE btnDumpChoose :
/*------------------------------------------------------------------------
  Name         : btnDumpChoose 
  Description  : Dump selected records
  ---------------------------------------------------------------------- 
  18-03-2009 pti Created
  ----------------------------------------------------------------------*/

  define variable cSetting       as character   no-undo.
  define variable cOldDateFormat as character   no-undo.

  /* If there is no record selected, select the focused one */
  if valid-handle(ghDataBrowse) 
    and ghDataBrowse:num-selected-rows = 0 then
    ghDataBrowse:select-focused-row().

  /* When you start DataDigger from more than 1 environment, chances are
   * that you might start with different date-format settings. The dump 
   * window saves the date of the last dump in the session:date-format 
   * so this needs to be consistent throughout all runs of DataDigger.
   */
  cOldDateFormat = session:date-format.

  /* Check Date-format in ini file */
  cSetting = getRegistry('DataDigger', 'DateFormat').

  case cSetting:
    when ? then setRegistry('DataDigger', 'DateFormat', session:date-format).
    when session:date-format then .
    otherwise session:date-format = cSetting.
  end case. 

  run dumpData.

  /* Restore date format */
  session:date-format = cOldDateFormat.

END PROCEDURE. /* btnDumpChoose */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnEditChoose C-Win 
PROCEDURE btnEditChoose :
/*------------------------------------------------------------------------
  Name         : btnEditChoose
  Description  : Edit one or more records in a separate window

  ----------------------------------------------------------------------
  14-01-2011 pti Created
  ----------------------------------------------------------------------*/

  define variable lRecordsUpdated as logical no-undo. 
  define buffer ttField for ttField. 

  /* If there is no record selected, select the focused one */
  if ghDataBrowse:num-selected-rows = 0 then
    ghDataBrowse:select-focused-row().

  run value(getProgramDir() + 'wEdit.w')
    ( input ghDataBrowse
    , input gcCurrentDatabase
    , input gcCurrentTable
    , input table ttField /* do not use by-reference */
    , output lRecordsUpdated
    ).

  if lRecordsUpdated then ghDataBrowse:refresh().

end procedure. /* btnEditChoose */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnFavouritesChoose C-Win 
PROCEDURE btnFavouritesChoose :
/*------------------------------------------------------------------------
  Name         : btnFavouritesChoose
  Description  : Maintenance of database connection settings

  ----------------------------------------------------------------------
  14-01-2011 pti Created
  ----------------------------------------------------------------------*/

  define variable cDummy        as character   no-undo.
  define variable cProgDir      as character   no-undo.
  define variable cDatabases    as character   no-undo.
  define variable cDatabasesOld as character   no-undo.
  define variable cCurrentDb    as character   no-undo. 

  cProgDir   = getProgramDir().
  cCurrentDb = getCurrentDatabase(). 

  cDatabasesOld = getDatabaseList().
  run value(cProgDir + 'wConnections.w') (input 'UI', input '', output cDummy).
  /* run setConnectionMenu. */

  /* Get all connected databases */
  cDatabases = getDatabaseList().

  /* If needed, repopulate db combo */
  if cDatabases <> cDatabasesOld then 
  do:
    /* Get list of all tables of all databases */
    run getTables(output table ttTable).
    assign cbDatabaseFilter:list-items in frame frMain = ',' + cDatabases.

    apply 'choose' to btnTableFilter. 
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnQueriesChoose C-Win 
PROCEDURE btnQueriesChoose :
/*------------------------------------------------------------------------
  Name         : btnQueriesChoose
  Description  : Maintenance of database connection settings

  ----------------------------------------------------------------------
  14-01-2011 pti Created
  ----------------------------------------------------------------------*/

  define variable iQuery  as integer no-undo. 
  define variable hEditor as handle  no-undo.

  hEditor = getActiveQueryEditor().

  run value(getProgramDir() + 'dQueries.w')
    ( input getCurrentDatabase()
    , input getCurrentTable()
    , input hEditor:screen-value
    , output iQuery
    ).

  if iQuery = ? then 
    return.
  else 
  do:
    /* Queries might be changed, so reload them */
    run collectQueryInfo
      ( input getCurrentDatabase()
      , input getCurrentTable() 
      ).
    giQueryPointer = iQuery.
    setQuery(0).
  end.

END PROCEDURE. /* btnQueriesChoose */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnSettingsChoose C-Win 
PROCEDURE btnSettingsChoose :
/*------------------------------------------------------------------------
  Name         : btnSettingsChoose
  Description  : Show DataDigger settings window

  ----------------------------------------------------------------------
  14-01-2011 pti Created
  ----------------------------------------------------------------------*/

  define variable cEnvironment as character   no-undo.
  define variable lOkClicked   as logical     no-undo.

  /* Load or create personalized ini file */
  cEnvironment = substitute('&1DataDigger-&2.ini'
                           , getProgramDir()
                           , getUserName() 
                           ).

  run value(getProgramDir() + '\dSettings.w') 
     ( input cEnvironment
     , output lOkClicked
     ).

  if lOkClicked then run initializeObjects. 

END PROCEDURE. /* btnSettingsChoose */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnViewChoose C-Win 
PROCEDURE btnViewChoose :
/*------------------------------------------------------------------------
  Name         : btnViewChoose
  Description  : Show a record in a more readable format in a new window. 

  ----------------------------------------------------------------------
  16-01-2009 pti Created
  27-08-2010 pti Rewritten to show multiple records.
  ----------------------------------------------------------------------*/

  define variable cFileType   as character   no-undo.
  define variable hDataBuffer as handle      no-undo.
  define variable iMaxWidth   as integer     no-undo. 
  define variable cDataFormat as character   no-undo.
  define variable iFieldCount as integer     no-undo.
  define variable iRecord     as integer     no-undo.
  define variable cLineStart  as character   no-undo.
  define variable cLabelEnd   as character   no-undo.
  define variable cColumnSep  as character   no-undo.
  define variable cLineEnd    as character   no-undo.
  define variable cDocStart   as character   no-undo.
  define variable cDocEnd     as character   no-undo.
  define variable cFilename   as character   no-undo.
  define variable cLabelStart as character   no-undo.
  define variable cDataStart  as character   no-undo extent 2.
  define variable cDataEnd    as character   no-undo.
  define variable iLineNr     as integer     no-undo.

  define buffer bView for ttView. 
  define buffer ttField for ttField. 

  /* If there is no record selected, select the focused one */
  if ghDataBrowse:num-selected-rows = 0 then
    ghDataBrowse:select-focused-row().

  /* Is a type set? */
  if getRegistry('DataDigger', 'ViewType') = ? then
  do:
    run showHelp('NoViewTypeSet', '').
    run setViewType('txt').
  end.

  /* What type do we want? */
  cFileType = getRegistry('DataDigger', 'ViewType').

  /* Cleanup */
  empty temp-table ttView. 
  empty temp-table ttColumnWidth.

  /* Get data */
  if not valid-handle(ghDataBrowse) then return.
  hDataBuffer = ghDataBrowse:query:get-buffer-handle(1).
  if not hDataBuffer:available then return.

  collectLoop:
  for each ttField 
    where ttField.lShow      = true
      and ttField.lDataField = true
      and ttField.cFieldName <> 'RECID'
      and ttField.cFieldName <> 'ROWID' 
    break by ttField.cFieldname
          by ttField.iExtent:

    iFieldCount = iFieldCount + 1.

    create ttView.
    assign ttView.iColumnNr   = 0
           ttView.iFieldNr    = iFieldCount
           ttView.cValue      = ttField.cFullName
           ttView.cColumnName = ttField.cFullName
           .

    /* Walk thru all selected records */
    do iRecord = 1 to ghDataBrowse:num-selected-rows:
      ghDataBrowse:fetch-selected-row(iRecord).

      create ttView.
      assign ttView.iColumnNr   = iRecord
             ttView.iFieldNr    = iFieldCount
             ttView.cValue      = string(hDataBuffer:buffer-field(ttField.cFieldName):buffer-value(ttField.iExtent), ttField.cFormat )
             ttView.cColumnName = ttField.cFullName
             .
    end. /* iRecord */
  end. /* for each ttField */

  /* Calculate maximum width per column */
  do iRecord = 0 to ghDataBrowse:num-selected-rows:

    /* Find out maximum width of all elements in this col */
    iMaxWidth = 0.
    for each bView where bView.iColumnNr = iRecord:
      iMaxWidth = maximum(iMaxWidth, length(bView.cValue)).
    end.

    create ttColumnWidth.
    assign ttColumnWidth.iColumnNr = iRecord
           ttColumnWidth.iWidth    = iMaxWidth. 
  end.

  /* Determine a unique filename 
   * Something like: datadigger-view-customer.txt
   */
  cFilename = substitute('&1datadigger-view.&2'
                        , session:temp-dir
                        , cFileType
                        ).

  /* Showtime! */
  if search(cFileName) <> ?
    and isFileLocked(cFileName) then
  do:
    message 'Error opening temporary file.~nDo you have it open for editing?~n~n' cFilename view-as alert-box info buttons ok.
    return.
  end.

  output to value( cFilename ).

  case cFileType:
    when 'txt'  then assign cDocStart     = ''
                            cLineStart    = '~n'
                            cLabelStart   = ''      cLabelEnd  = ' = '
                            cDataStart[1] = ''      cDataEnd   = ' | '
                            cDataStart[2] = ''      
                            cLineEnd      = ''
                            cDocEnd       = ''
                            .
    when 'xls' or 
    when 'html' then assign cDocStart     = '<html><body><table border=1>'
                            cLineStart    = '~n<tr>'
                            cLabelStart   = '<td bgcolor="KHAKI"><b>'         cLabelEnd   = '</b></td>'
                            cDataStart[1] = '<td bgcolor="LIGHTYELLOW">'      cDataEnd    = '&nbsp;</td>'
                            cDataStart[2] = '<td bgcolor="WHITE">'      
                            cLineEnd      = '</td> </tr>'
                            cDocEnd       = '~n</table></body></html>'
                            .
  end case. 

  put unformatted cDocStart.
  for each ttView 
    break by ttView.iFieldNr:

    /* Determine format for data to get names aligned */
    find ttColumnWidth where ttColumnWidth.iColumnNr = ttView.iColumnNr.
    cDataFormat = fill('x', ttColumnWidth.iWidth).

    if first-of(ttView.iFieldNr) then
    do:
      iLineNr = iLineNr mod 2 + 1.
      put unformatted cLineStart cLabelStart string(ttView.cValue,cDataFormat) cLabelEnd.
    end.
    else
      put unformatted cDataStart[iLineNr] string(ttView.cValue,cDataFormat) cDataEnd.

    if last-of(ttView.iFieldNr) then 
      put unformatted cLineEnd.
  end.
  put unformatted cDocEnd.
  output close. 

  /* Start associated program for the txt file */
  os-command no-wait start value(cFilename).

  /* Cleanup */
  empty temp-table ttView. 
  empty temp-table ttColumnWidth.

END PROCEDURE. /* btnViewChoose */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE collectFieldInfo C-Win 
PROCEDURE collectFieldInfo PRIVATE :
/*------------------------------------------------------------------------
  Name         : collectFieldInfo 
  Description  : Fill the fields temp-table
  ---------------------------------------------------------------------- 
  18-03-2009 pti Created
  ----------------------------------------------------------------------*/

  define input parameter pcTableName   as character   no-undo.


  define buffer ttField for ttField. 


  /* Collect fields from target table */
  run getFields( input gcCurrentDatabase
               , input pcTableName
               , output table ttField
               ).


end procedure. /* collectFieldInfo */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE collectIndexInfo C-Win 
PROCEDURE collectIndexInfo :
/*------------------------------------------------------------------------
  Name         : collectIndexInfo
  Description  : Fill the index temp-table
  ---------------------------------------------------------------------- 
  01-09-2009 pti Created
  ----------------------------------------------------------------------*/

  define input parameter pcTableName   as character   no-undo.

  define variable hBufferFile       as handle      no-undo.
  define variable hBufferIndex      as handle      no-undo.
  define variable hBufferIndexField as handle      no-undo.
  define variable hBufferField      as handle      no-undo.
  define variable cQuery            as character   no-undo. 
  define variable hQuery            as handle      no-undo.

  /* Return if no db connected */
  if num-dbs = 0 then return. 

  /*
  ** Fill the tt with _Fields
  */
  create buffer hBufferFile       for table gcCurrentDatabase + "._File".                    
  create buffer hBufferIndex      for table gcCurrentDatabase + "._Index".
  create buffer hBufferIndexField for table gcCurrentDatabase + "._Index-Field".
  create buffer hBufferField      for table gcCurrentDatabase + "._Field".

  create query hQuery.
  hQuery:set-buffers(hBufferFile,hBufferIndex,hBufferIndexField,hBufferField).

  cQuery = substitute("for each &1._File  where &1._file._file-name = '&2' no-lock " +
                      "  , each &1._Index       of &1._File        no-lock " + 
                      "  , each &1._Index-field of &1._Index       no-lock " + 
                      "  , each &1._Field       of &1._Index-field no-lock where true " 
                     , gcCurrentDatabase
                     , pcTableName
                     ).

  hQuery:query-prepare(cQuery).
  empty temp-table ttIndex.
  hQuery:query-open().
  hQuery:get-first().

  repeat while not hQuery:query-off-end:

    find ttIndex where ttIndex.cIndexName = hBufferIndex:buffer-field('_index-name'):buffer-value no-error.
    if not available ttIndex then 
    do:
      create ttIndex.

      ttIndex.cIndexName  = hBufferIndex:buffer-field('_index-name'):buffer-value.
      ttIndex.cIndexFlags = string( hBufferFile:buffer-field('_prime-index'):buffer-value = hBufferIndex:recid, 'P/') 
                          + string( hBufferIndex:buffer-field('_unique'):buffer-value, ' U/') 
                          + string( hBufferIndex:buffer-field('_WordIdx'):buffer-value <> ?, ' W/') 
                          + string( hBufferIndex:buffer-field('_Active'):buffer-value , ' /INACTIVE') 
                          .
      ttIndex.cIndexFlags  = trim(ttIndex.cIndexFlags).
      ttIndex.lIndexActive = hBufferIndex:buffer-field('_Active'):buffer-value.
    end.

    /* Add field */
    ttIndex.cIndexFields = substitute('&1  &2&3'
                                     , ttIndex.cIndexFields 
                                     , hBufferField:buffer-field('_field-name'):buffer-value 
                                     , string( hBufferIndexField:buffer-field('_Ascending'):buffer-value, '+/-')
                                     ).
    ttIndex.cIndexFields = trim(ttIndex.cIndexFields,' ').

    /* Naked list of just fieldnames */
    ttIndex.cFieldList   = substitute('&1,&2'
                                     , ttIndex.cFieldList 
                                     , hBufferField:buffer-field('_field-name'):buffer-value 
                                     ).
    ttIndex.cFieldList   = trim(ttIndex.cFieldList,', ').

    hQuery:get-next().
  end.
  hQuery:query-close().

  delete object hQuery.
  delete object hBufferFile.      
  delete object hBufferIndex.     
  delete object hBufferIndexField.
  delete object hBufferField.     

end procedure. /* collectIndexInfo */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE connectDatabase C-Win 
PROCEDURE connectDatabase :
/*------------------------------------------------------------------------
  Name         : connectDatabase
  Description  : Connect to a database via the context menu
  ---------------------------------------------------------------------- 
  18-09-2009 pti Created
  ----------------------------------------------------------------------*/

  define input parameter pcDatabase as character no-undo.

  define variable cError        as character   no-undo.
  define variable cProgDir      as character   no-undo.
  define variable cDatabases    as character   no-undo.
  define variable cDatabasesOld as character   no-undo.
  define variable cCurrentDb    as character   no-undo. 

  do with frame {&frame-name}:
    cProgDir   = getProgramDir().
    cCurrentDb = getCurrentDatabase(). 

    cDatabasesOld = getDatabaseList().
    run value(cProgDir + 'wConnections.w') (input 'CONNECT', input pcDatabase, output cError).
    if cError <> '' then
      message cError view-as alert-box info buttons ok.

    /* Rebuild context menu */
    /* run setConnectionMenu. */

    /* Get all connected databases */
    cDatabases = getDatabaseList().

    /* If needed, repopulate db combo */
    if cDatabases <> cDatabasesOld then
    do:
      assign 
        cbDatabaseFilter:list-items   = ',' + cDatabases
        cbDatabaseFilter:screen-value = cCurrentDb
        .

      /* Get list of all tables of all databases */
      run getTables(output table ttTable).
    end.

    /* If the chosen DB is connected, switch to that one */
    if lookup(pcDatabase, cDatabases) > 0 then
      cbDatabaseFilter:screen-value = pcDatabase.

    apply 'value-changed' to cbDatabaseFilter. 
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "wDataDigger.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
    chFilterTimer = FilterTimer:COM-HANDLE
    UIB_S = chFilterTimer:LoadControls( OCXFile, "FilterTimer":U)
    FilterTimer:NAME = "FilterTimer":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "wDataDigger.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyToClipboard C-Win 
PROCEDURE copyToClipboard :
/*------------------------------------------------------------------------
  Name         : copyToClipboard 
  Description  : Copy the value of the column to the clipboard
  ---------------------------------------------------------------------- 
  22-09-2010 pti Created
  ----------------------------------------------------------------------*/

  define variable cColumnName  as character   no-undo.
  define variable cColumnValue as character   no-undo.

  if num-entries(ghDataBrowse:private-data,chr(1)) <> 3 then return. 

  cColumnName  = entry(1, ghDataBrowse:private-data,chr(1)).
  cColumnValue = entry(2, ghDataBrowse:private-data,chr(1)).

  if cColumnValue <> '' and cColumnValue <> ? then clipboard:value = trim(cColumnValue).

end procedure. /* copyToClipboard */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createCounter C-Win 
PROCEDURE createCounter :
/*------------------------------------------------------------------------
  Name         : createCounter
  Description  : Create the digits for the counter
  ---------------------------------------------------------------------- 
  25-01-2011 pti Created
  ----------------------------------------------------------------------*/

  define variable iDigit     as integer no-undo. 
  define variable iNumDigits as integer no-undo initial {&numDigits}. 
  define variable hText      as handle  no-undo. 
  define variable iRow       as integer no-undo. 

  do with frame {&frame-name}:

    do iRow = 1 to 2:
      do iDigit = 1 to iNumDigits:
        create text hText
          assign 
            frame         = frame {&frame-name}:handle
            screen-value  = "0"
            font          = 1
            fgcolor       = 7
            x             = rcCounter:x + {&marginHor} + (iDigit - 1) * {&digitWidth}
            y             = rcCounter:y + {&marginVer} - 1
            width-pixels  = {&digitWidth}
            height-pixels = {&digitHeight}
            visible       = yes
            sensitive     = no
            .
        case iRow:
          when 1 then ghNewDigit[iDigit] = hText.
          when 2 then ghOldDigit[iDigit] = hText.
        end case. 

      end.
    end.  

    rcCounter:width-pixels  = iNumDigits * {&digitWidth} + 2 * {&marginHor}.
    rcCounter:height-pixels = {&digitHeight} + 2 * {&marginVer}.
    rcCounter:visible = no.
  end.

END PROCEDURE. /* createCounter */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createMenuDataBrowse C-Win 
PROCEDURE createMenuDataBrowse :
/*------------------------------------------------------------------------
  Name         : createMenuDataBrowse2
  Description  : Rebuild the connection submenu of the 'add' button

  ----------------------------------------------------------------------
  18-09-2009 pti Created
  ----------------------------------------------------------------------*/

  /* Attach connections to btnConnect */
  define variable hMenu     as handle      no-undo.
  define variable hMenuItem as handle      no-undo.
  define variable iColumn   as integer     no-undo. 
  define variable hColumn   as handle      no-undo. 

  hMenu = ghDataBrowse:popup-menu.

  /* Kill the current menu */
  if valid-handle(hMenu) then
  do:
    /* Kill subitems */
    do while valid-handle(hMenu:first-child):
      delete object hMenu:first-child.
    end.

    /* Kill the menu itself */
    delete object hMenu.
  end.

  /* If there is nothing, then just return */
  if ghDataBrowse:query:num-results = 0 then return.

  /* Create the menu itself */
  create menu hMenu
    assign
      popup-only = true
      sensitive = true
    triggers:
      on 'menu-drop' persistent run menuDropDataBrowse in this-procedure. /* enable/disable menu-items */
    end triggers.

  ghDataBrowse:popup-menu = hMenu.

  /* Create menu-items. The labels will be set in the drop-menu trigger using the text in private-data */
  /* Copy to clipboard */
  create menu-item hMenuItem
    assign
      private-data = 'Copy to clipboard'
      name         = 'copyToClipboard'
      parent       = hMenu
      sensitive    = true
    triggers:
      on 'CHOOSE':U persistent run copyToClipboard in this-procedure.
    end triggers.

  /* Show full value */
  create menu-item hMenuItem
    assign
      private-data = 'Show Value'
      name         = 'showValue'
      parent       = hMenu
      sensitive    = true
    triggers:
      on 'CHOOSE':U persistent run showValue in this-procedure.
    end triggers.


  /* Add this field as filter */
  create menu-item hMenuItem
    assign
      /* private-data = 'Add filter: "<field>" = "<value>"' */
      private-data = 'Add to filter'
      name         = 'addFilter'
      parent       = hMenu
      sensitive    = true
    triggers:
      on 'CHOOSE':U persistent run setDataFilter in this-procedure (no).
    end triggers.

  /* Filter on this field only */
  create menu-item hMenuItem
    assign
      /* private-data = 'Set filter: "<field>" = "<value>"' */
      private-data = 'Set as filter'
      name         = 'setFilter'
      parent       = hMenu
      sensitive    = true
    triggers:
      on 'CHOOSE':U persistent run setDataFilter in this-procedure (yes).
    end triggers.

  /* Filter on this field only */
  create menu-item hMenuItem
    assign
      private-data = 'Clear Filters'
      name         = 'clearFilter'
      parent       = hMenu
      sensitive    = true
    triggers:
      on 'CHOOSE':U persistent run btnClearDataFilterChoose in this-procedure.
    end triggers.

  /* Separator */
  create menu-item hMenuItem
    assign
      parent       = hMenu
      subtype      = 'rule'
      .

  /* Shortcut to viewing records */
  create menu-item hMenuItem
    assign
      label        = ''
      name         = 'view'
      private-data = 'View selected' /*  records (<NumSel>) */
      parent       = hMenu
    triggers:
      on 'CHOOSE':U persistent run btnViewChoose in this-procedure.
    end triggers.

  /* Shortcut to editing records */
  create menu-item hMenuItem
    assign
      label        = ''
      name         = 'edit'
      private-data = 'Edit selected' /* records (<NumSel>) */
      parent       = hMenu
    triggers:
      on 'CHOOSE':U persistent run btnEditChoose in this-procedure.
    end triggers.

  /* Shortcut to dumping records */
  create menu-item hMenuItem
    assign
      label        = ''
      name         = 'dump'
      private-data = 'Dump selected' 
      parent       = hMenu
    triggers:
      on 'CHOOSE':U persistent run btnDumpChoose in this-procedure.
    end triggers.

  /* Separator */
  create menu-item hMenuItem
    assign
      parent       = hMenu
      subtype      = 'rule'
      .

  /* Shortcut to hiding the column */
  create menu-item hMenuItem
    assign
      label        = ''
      name         = 'hideColumn'
      private-data = 'Hide this column' 
      parent       = hMenu
    triggers:
      on 'CHOOSE':U persistent run hideColumn in this-procedure.
    end triggers.

  /* Shortcut to unhiding the column */
  create menu-item hMenuItem
    assign
      label        = ''
      name         = 'unhideColumn'
      private-data = 'Unhide all columns' 
      parent       = hMenu
    triggers:
      on 'CHOOSE':U persistent run showField in this-procedure('*',true).
    end triggers.

  /* Separator */
  create menu-item hMenuItem
    assign
      parent       = hMenu
      subtype      = 'rule'
      .

  /* Shortcut to deleting records */
  create menu-item hMenuItem
    assign
      label        = ''
      name         = 'delete'
      private-data = 'Delete selected'
      parent       = hMenu
    triggers:
      on 'CHOOSE':U persistent run btnDeleteChoose in this-procedure.
    end triggers.


end procedure. /* createMenuDataBrowse */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataColumnEntry C-Win 
PROCEDURE dataColumnEntry :
/*------------------------------------------------------------------------
  Name         : dataColumnEntry
  Description  : Enter a datarow
  ---------------------------------------------------------------------- 
  18-09-2009 pti Created
  ----------------------------------------------------------------------*/

  glRowEditActive = yes.
  self:private-data = self:screen-value.

  /* Activate buttons */
  setUpdatePanel('update').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataColumnLeave C-Win 
PROCEDURE dataColumnLeave :
/*------------------------------------------------------------------------
  Name         : dataColumnLeave
  Description  : Leave a datarow
  ---------------------------------------------------------------------- 
  18-09-2009 pti Created
  ----------------------------------------------------------------------*/


  glRowEditActive = no.

  /* Activate buttons */
  setUpdatePanel('display').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataColumnSort C-Win 
PROCEDURE dataColumnSort PRIVATE :
/*------------------------------------------------------------------------
  Name         : dataColumnSort
  Description  : Sort on a datacolumn
  ---------------------------------------------------------------------- 
  18-09-2009 pti Created
  07-02-2011 pti Rewritten
  ----------------------------------------------------------------------*/

  run reopenDataBrowse(self:current-column:name,?).

END PROCEDURE. /* dataColumnSort */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DataDiggerClose C-Win 
PROCEDURE DataDiggerClose :
/*------------------------------------------------------------------------
  Name         : DataDiggerClose
  Description  : Close DataDigger after event 'DataDiggerClose'
  ---------------------------------------------------------------------- 
  14-12-2009 pti Created
  ----------------------------------------------------------------------*/
  /* wfk - only allow one instance */
  /* DELETE OBJECT hDiggerLib. */
  apply 'close' to this-procedure. 

end procedure. /* DataDiggerClose */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataDoubleClick C-Win 
PROCEDURE dataDoubleClick :
/*------------------------------------------------------------------------
  Name         : dataDoubleClick
  Description  : Double click on databrowse might result in 
                 EDIT / VIEW / DUMP

  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/
  define input parameter hBrowseBuffer as handle no-undo.

  define variable cAction as character   no-undo.

  cAction = getRegistry('DataDigger','DataDoubleClick').

  /* Is a type set? */
  if cAction = ? then
  do:
    run showHelp('NoDataDoubleClickSet', '').
    cAction = 'VIEW'.
    setRegistry('DataDigger','DataDoubleClick',cAction).

    /* If the user does not have a default view type set, don't show two dialogs 
     * right after each other. Just set the viewtype to TXT then.
     */
    if getRegistry('DataDigger', 'ViewType') = ? then
      run setViewType('txt').
  end.

  case cAction:
    when 'VIEW' then run btnViewChoose. 
    when 'EDIT' then run btnEditChoose. 
    when 'DUMP' then run btnDumpChoose. 
  end case.

end procedure. /* dataDoubleClick */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataOffHome C-Win 
PROCEDURE dataOffHome :
define buffer ttField for ttField. 

  find ttField where ttField.cFullName = gcLastDataField no-error.
  if not available ttField then find first ttField. 
  if not available ttField then return. 

  setFilterFieldColor(ttField.hFilter).
  apply 'entry' to ttField.hFilter. 

  return no-apply.

end procedure. /* dataOffHome */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataRowDisplay C-Win 
PROCEDURE dataRowDisplay :
/*------------------------------------------------------------------------
  Name         : dataRowDisplay
  Description  : Set the background color to another color to get 
                 an odd/even coloring of the rows.

  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/
  define input parameter hBrowseBuffer as handle no-undo.

  define buffer ttField for ttField.

  for each ttField:
    if not valid-handle(ttField.hColumn) then next.

    /* Alternate FG and BGcolor */
    ttField.hColumn:bgcolor = (if hBrowseBuffer:query:current-result-row modulo 2 <> 1 then ? else 8).
    ttField.hColumn:fgcolor = 1.

    /* add RECID */
    if ttField.cFieldName = 'RECID' then
    do:
      ttField.hColumn:font = 2.
      ttField.hColumn:screen-value = string( hBrowseBuffer:recid, 'zzzzzzzzz9' ).
    end.

    /* add ROWID */
    if ttField.cFieldName = 'ROWID' then
    do:
      ttField.hColumn:font = 2.
      ttField.hColumn:screen-value = string( hBrowseBuffer:rowid, 'x(30)' ).
    end.

    if ttField.cFormat begins 'hh:' then
    do:
      assign 
        ttField.hColumn:screen-value = string( hBrowseBuffer:buffer-field(ttField.cFieldName):buffer-value(ttField.iExtent), ttField.cFormat ) no-error.

      /* If you type a crappy time format like HH:MAM:SS just ignore it */
      if error-status:error then
        ttField.hColumn:screen-value = string( hBrowseBuffer:buffer-field(ttField.cFieldName):buffer-value(ttField.iExtent) ).
    end.


  end.

end procedure. /* dataRowDisplay */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataRowJumpToEnd C-Win 
PROCEDURE dataRowJumpToEnd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER hBrowseBuffer AS HANDLE      NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataRowLeave C-Win 
PROCEDURE dataRowLeave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  define variable lSave as logical     no-undo.



  do with frame {&frame-name}:

    /* Not allowed to go to another row while editing */
    if valid-handle(last-event:widget-enter)
      and valid-handle(last-event:widget-enter:parent)
      and last-event:widget-enter:parent = ghDataBrowse 
      and ghDataBrowse:modified then 
    do:
      message 'Save changes?' view-as alert-box info buttons yes-no-cancel update lSave.

      if lSave = yes then 
      do:
        ghDataBrowse:modified = no.
        apply 'choose' to btnEdit.
        apply 'leave' to focus.
      end.

      else 
      if lSave = no then 
      do:
        ghDataBrowse:modified = no.
        apply 'leave' to focus.
      end.

      else 
      return no-apply.
    end.

  end.


/* IF ghDataBrowse:NEW-ROW THEN                    */
/*     DO ON ERROR UNDO, RETURN NO-APPLY:     */
/*       ghDataBrowse:CREATE-RESULT-LIST-ENTRY().  */
/*     END.                                   */
/*                                            */

/* ghDataBrowse:REFRESH(). */

END PROCEDURE. /* dataRowLeave */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataRowValueChanged C-Win 
PROCEDURE dataRowValueChanged :
/*------------------------------------------------------------------------
  Name         : dataRowValueChanged
  Description  : Save the content of the fields in linkinfo

  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER hBrowseBuffer AS HANDLE      NO-UNDO.

  define variable iColumn    as integer     no-undo.
  define variable hColumn    as handle      no-undo.
  define variable cFieldName as character   no-undo. 

  do iColumn = 1 to num-entries(gcDataBrowseColumns):

    hColumn    = widget-handle( entry(iColumn,gcDataBrowseColumns) ).
    cFieldName = entry(iColumn,gcDataBrowseColumnNames).

    if cFieldName = 'RECID' or cFieldName = 'ROWID' then
      cFieldName = lower(substitute('&1 - &2', getCurrentTable(), cFieldName)).

    setLinkInfo(cFieldName, hColumn:screen-value).
  end.

  setUpdatePanel(?). /* Refresh sensitivity of buttons if needed */

end procedure. /* dataRowValueChanged */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataScrollNotify C-Win 
PROCEDURE dataScrollNotify :
/*----------------------------------------------------------------------------
  Naam         : dataScrollNotify

  Omschrijving : Afmetingen en posities van filterfields net zo zetten 
                 als de columns in de browse

  Parameters   : phBrowse = Handle van de browse

  ------------------------------------------------------------------------- */
  define input  parameter phBrowse as handle     no-undo.

  define variable cFilterFields as character   no-undo.
  define variable iColumn       as integer     no-undo.
  define variable cButtons      as character   no-undo.

  define buffer ttField for ttField. 

  /* Might get called when browse is not yet realized, so: */
  if not valid-handle(phBrowse) then return.

  /* Freeze all */
  setWindowFreeze(yes).

  getFilterLoop:
  for each ttField 
    where ttField.lDataField = true
      and valid-handle(ttField.hFilter):
    cFilterFields = trim(substitute('&1,&2', cFilterFields, ttField.hFilter),',').
  end.

/*   getFilterLoop:                                                                       */
/*   do iColumn = 1 to phBrowse:num-columns:                                              */
/*                                                                                        */
/*     /* Find the field */                                                               */
/*     find ttField where ttField.hColumn = phBrowse:get-browse-column(iColumn) no-error. */
/*     if not available ttField or ttField.cDataType = 'raw' then next.                   */
/*                                                                                        */
/*     cFilterFields = trim(substitute('&1,&2', cFilterFields, ttField.hFilter),',').     */
/*   end.                                                                                 */

  do with frame {&frame-name}:
    cButtons = substitute('&1,&2', btnClearDataFilter:handle, btnDataFilter:handle).
  end.

  /* Resize them */
  run resizeFilterFields
    ( input cFilterFields
    , input cButtons
    , input phBrowse
    ).

  run showScrollBars(frame {&frame-name}:handle, no, no).
  setWindowFreeze(no).

end procedure.  /* dataScrollNotify */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataSelectAll C-Win 
PROCEDURE dataSelectAll :
/*----------------------------------------------------------------------------
  Name: dataSelectAll
  Desc: Select all records in the browse
  ------------------------------------------------------------------------- */
  define input  parameter phBrowse as handle     no-undo.

  setWindowFreeze(yes).
  phBrowse:select-all().
  setWindowFreeze(no).

end procedure. /* dataSelectAll */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataSelectNone C-Win 
PROCEDURE dataSelectNone :
/*----------------------------------------------------------------------------
  Name: dataSelectNone
  Desc: Deselect all records in the browse
  ------------------------------------------------------------------------- */
  define input  parameter phBrowse as handle     no-undo.

  setWindowFreeze(yes).
  phBrowse:deselect-rows().
  setWindowFreeze(no).

end procedure. /* dataSelectNone */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteDataFilters C-Win 
PROCEDURE deleteDataFilters :
/*----------------------------------------------------------------------------
  Naam         : deleteDataFilters

  Omschrijving : Kill the data filters and kill scrollbars

  ------------------------------------------------------------------------- */
  define buffer ttField for ttField. 

  for each ttField:
    delete object ttField.hFilter no-error.
  end.

end procedure. /* deleteDataFilters */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dumpData C-Win 
PROCEDURE dumpData :
/*------------------------------------------------------------------------
  Name         : dumpData 
  Description  : Dump the records in the data browse to file.
  ---------------------------------------------------------------------- 
  18-03-2009 pti Created
  27-10-2009 beu Implemented dump dialog with XML-format
  ----------------------------------------------------------------------*/

  define buffer ttField for ttField. 

  run value(getProgramDir() + 'dDump.w')
    ( input ghDataBrowse
    , input getSelectedFields()
    , input table ttField by-reference
    ).

end procedure. /* dumpData */

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
  RUN control_load.
  DISPLAY fiOrderFilter fiNameFilter fiTypeFilter fiFormatFilter fiLabelFilter 
          fiIndexNameFilter fiFlagsFilter fiFieldsFilter tgSelAll fiTableFilter 
          cbDatabaseFilter fiNumQueriesFilter fiLastUsedFilter fiTableDesc 
          ficWhere fiIndexInfo fiNumresults 
      WITH FRAME frMain IN WINDOW C-Win.
  ENABLE btnTabFields rctData rctQuery rctEdit rcCounter btnDataDigger btnTools 
         btnHelp btnQueryTester fiOrderFilter fiNameFilter fiTypeFilter 
         fiFormatFilter fiLabelFilter btnClearFieldFilter btnFieldFilter 
         fiIndexNameFilter fiFlagsFilter fiFieldsFilter btnTabIndexes 
         btnClearIndexFilter btnIndexFilter tgSelAll brFields btnAddFilter 
         btnAddFilter2 brIndexes fiTableFilter cbDatabaseFilter 
         fiNumQueriesFilter fiLastUsedFilter btnClearTableFilter btnTableFilter 
         btnEditFilter btnEditFilter2 brTables btnMoveTop btnMoveUp btnReset 
         btnMoveDown btnMoveBottom fiTableDesc btnWhere btnClear btnPrevQuery 
         btnQueries btnNextQuery btnClipboard ficWhere btnClearDataFilter 
         btnDataFilter btnAdd btnEdit btnDelete btnView btnDump btnLoad 
         fiNumresults 
      WITH FRAME frMain IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frMain}
  DISPLAY cbAndOr cbFields cbOperator ficValue ficWhere2 
      WITH FRAME frWhere IN WINDOW C-Win.
  ENABLE btnAnd rctQueryButtons cbAndOr cbFields cbOperator ficValue btnInsert 
         ficWhere2 btnClear-2 btnPrevQuery-2 btnQueries-2 btnNextQuery-2 
         btnClipboard-2 btnBegins btnOK btnCancel-2 btnBracket btnContains 
         btnEq btnGT btnLT btnMatches btnNE btnOr btnQt btnToday 
      WITH FRAME frWhere IN WINDOW C-Win.
  VIEW FRAME frWhere IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frWhere}
  ENABLE btnSettings btnAbout-txt btnFavorites btnDump-2 btnLoad-2 btnQueries-3 
         btnChangeLog btnAbout btnChangeLog-txt btnDump-txt btnFavorites-txt 
         btnLoad-txt btnQueries-txt btnSettings-txt 
      WITH FRAME frSettings IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frSettings}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filterFieldAnyPrintable C-Win 
PROCEDURE filterFieldAnyPrintable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define input  parameter phField as handle      no-undo.

  setFilterFieldColor(phField).

end procedure. /* filterFieldAnyPrintable */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filterFieldCursorDown C-Win 
PROCEDURE filterFieldCursorDown :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define input parameter phFilterField  as handle      no-undo.
  define input parameter phBrowseField  as handle      no-undo.

  define buffer ttField for ttField. 

  /* Remember the field we escaped from */
  find ttField where ttField.hFilter = phFilterField.
  gcLastDataField = ttField.cFullName.

  apply 'leave' to phFilterField.
  apply 'entry' to phBrowseField.

  return no-apply.

end procedure. /* filterFieldCursorDown */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filterFieldEntry C-Win 
PROCEDURE filterFieldEntry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define input  parameter phFilterField as handle      no-undo.

  /* If you enter the field and you have not put in a filter, 
   * clear out the field so you can type something yourself
   */
  if phFilterField:screen-value = phFilterField:private-data then
    phFilterField:screen-value = ''.

  setFilterFieldColor(phFilterField).

end procedure. /* filterFieldEntry */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filterFieldLeave C-Win 
PROCEDURE filterFieldLeave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define input  parameter phFilterField as handle      no-undo.

  define buffer ttField for ttField. 

  /* Remember the field we escaped from */
  find ttField where ttField.hFilter = phFilterField.

  if   ttField.cFilterValue = '' 
    or ttField.cFilterValue = ? then 
  do:
    phFilterField:screen-value = phFilterField:private-data.
  end.

/*   if phFilterField:screen-value = '' then                    */
/*   do:                                                        */
/*     phFilterField:screen-value = phFilterField:private-data. */
/*   end.                                                       */

  setFilterFieldColor(phFilterField).

end procedure. /* filterFieldLeave */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filterFieldValueChanged C-Win 
PROCEDURE filterFieldValueChanged :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define input  parameter phFilterField as handle      no-undo.

  define buffer ttField for ttField. 
  find ttField where ttField.hFilter = phFilterField.

  if    ttField.hFilter:screen-value <> ttField.hFilter:private-data 
    and ttField.hFilter:screen-value <> '' 
    and ttField.hFilter:screen-value <> ? then
  do:
    ttField.cFilterValue = phFilterField:screen-value.

    setRegistry( substitute('DB:&1',getCurrentDatabase())
               , substitute('&1.&2:filter',getCurrentTable(),ttField.cFullName)
               , phFilterField:screen-value
               ).
  end.
  else
  do:
    ttField.cFilterValue = "".
    setRegistry( substitute('DB:&1',getCurrentDatabase())
               , substitute('&1.&2:filter',getCurrentTable(),ttField.cFieldName)
               , ?
               ).
  end.

end procedure. /* filterFieldValueChanged */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getColumnPosition C-Win 
PROCEDURE getColumnPosition :
/*----------------------------------------------------------------------------
  Naam         : getColumnPosition

  Omschrijving : Bereken de positie van een kolom uit de browse. Door 
                 verschuivingen en vergrotingen is dat niet meer heel 
                 simpel. 

  Parameters   : phBrowse      = Handle van de browse
                 phColumn      = naam van kolom
                 poLinkerkant  = x-coordinaat van de linkerkant
                 poBreedte     = breedte in pixels van de kolom

  ------------------------------------------------------------------------- */
  define input  parameter phBrowse       as handle     no-undo.
  define input  parameter phColumn       as handle     no-undo.
  define output parameter poLinkerkant   as integer    no-undo.
  define output parameter poBreedte      as integer    no-undo.

  define variable cPosition      as character   no-undo.
  define variable iBrowseBreedte as integer     no-undo.
  define variable iVeldBreedte   as integer     no-undo.
  define variable iVeldLinks     as integer     no-undo.
  define variable iVeldRechts    as integer     no-undo.

  iVeldLinks     = phColumn:x.
  iVeldRechts    = phColumn:x + phColumn:width-pixels.
  iVeldBreedte   = phColumn:width-pixels.
  iBrowseBreedte = phBrowse:width-pixels - 30.

  publish 'message' (44, substitute('iVeldLinks:&1   iVeldBreedte:&2  iVeldRechts:&3   iBrowseBreedte:&4'
                                    , iVeldLinks, iVeldBreedte, iVeldRechts, iBrowseBreedte) ).

  /* initieel onbekend */
  cPosition = '?'.

  /* Helemaal links van de browse */
  if    iVeldLinks = -1 then 
    assign 
      cPosition    = 'L'
      poLinkerkant = 0
      poBreedte    = 0.

  /* Begint links en eindigt in de browse */
  else 
  if    iVeldLinks < 0  
    and iVeldRechts <= iBrowseBreedte then 
    assign 
      cPosition    = 'LM'
      poLinkerkant = 1
      poBreedte    = iVeldBreedte - (iVeldLinks * -1).

  /* Begint links en eindigt rechts van de browse */
  else 
  if    iVeldLinks < 0 
    and iVeldRechts > iBrowseBreedte then 
    assign 
      cPosition    = 'LMR'
      poLinkerkant = 1
      poBreedte    = iBrowseBreedte.

  /* Begint en eindigt in de browse */
  else 
  if    iVeldLinks > 0  
    and iVeldRechts <= iBrowseBreedte then 
    assign 
      cPosition    = 'M'
      poLinkerkant = iVeldLinks
      poBreedte    = iVeldBreedte.

  /* Begint in de browse en eindigt rechts van de browse */
  else 
  if    iVeldLinks  > 0 
    and iVeldLinks  < iBrowseBreedte
    and iVeldRechts > iBrowseBreedte then 
    assign 
      cPosition    = 'MR'
      poLinkerkant = iVeldLinks
      poBreedte    = iBrowseBreedte - iVeldLinks.


  /* Begint en eindigt rechts van de browse */
  else 
  if    iVeldLinks >= iBrowseBreedte then 
    assign 
      cPosition    = 'R'
      poLinkerkant = 0
      poBreedte    = 0.

  publish "message" ( 44, substitute('Col &1, position: &2', phColumn:name, cPosition )).
end procedure. /* getColumnPosition */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hideColumn C-Win 
PROCEDURE hideColumn :
/*------------------------------------------------------------------------
  Name         : hideColumn
  Description  : Hide the current column
  ---------------------------------------------------------------------- 
  18-01-2011 pti Created
  ----------------------------------------------------------------------*/

  define variable cColumnClicked as character   no-undo.
  define variable cColumnValue   as character   no-undo.
  define variable iExtentNr      as integer     no-undo.

  if num-entries(ghDataBrowse:private-data,chr(1)) <> 3 then return. 
  assign
    cColumnClicked = entry(1, ghDataBrowse:private-data,chr(1))
    cColumnValue   = entry(2, ghDataBrowse:private-data,chr(1))
    iExtentNr      = integer(entry(3, ghDataBrowse:private-data,chr(1)))
    .

  run showField(cColumnClicked,false).

end procedure. /* hideColumn */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE incQueriesOfTable C-Win 
PROCEDURE incQueriesOfTable :
/*------------------------------------------------------------------------
  Name         : incQueriesOfTable 
  Description  : Increment the number of queries served for a table.
                 This must be done in one move by fetching the nr 
                 from the ini file, adding one and saving it back since
                 the user could have more than one window open. 
  ----------------------------------------------------------------------
  17-11-2009 pti Created
  ----------------------------------------------------------------------*/

  define input parameter pcDatabase     as character   no-undo.
  define input parameter pcTable        as character   no-undo.
  define input parameter piNumIncrement as integer no-undo. 

  define variable iQueriesServed as integer   no-undo.
  define buffer bTable for ttTable.

  /* Which table? */
  find bTable 
    where bTable.cDatabase  = pcDatabase
      and bTable.cTableName = pcTable
          no-error.
  if not available bTable then return.

  /* Current number of queries served */
  iQueriesServed = integer( getRegistry( substitute('DB:&1', pcDatabase)
                                       , substitute('&1:QueriesServed', pcTable)
                                       )
                          ).
  if iQueriesServed = ? then iQueriesServed = 0.
  iQueriesServed = iQueriesServed + piNumIncrement.

  /* Save */
  assign 
    bTable.iNumQueries = iQueriesServed
    bTable.tLastUsed   = now.

  /* Save in registry */
  setRegistry ( substitute('DB:&1', pcDatabase )
              , substitute('&1:QueriesServed', pcTable )
              , string(bTable.iNumQueries) 
              ).
  setRegistry ( substitute('DB:&1', pcDatabase )
              , substitute('&1:LastUsed', pcTable )
              , string(bTable.tLastUsed, '99/99/9999 HH:MM:SS') 
              ).

  browse brTables:refresh().

end procedure. /* incQueriesOfTable */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE incQueriesServed C-Win 
PROCEDURE incQueriesServed :
/*------------------------------------------------------------------------
  Name         : incQueriesServed 
  Description  : Increment the number of queries served. We need to do 
                 this in one move by fetching the nr of queries served 
                 from the ini file, adding one and saving it back since
                 the user could have more than one window open. 
  ----------------------------------------------------------------------
  02-09-2009 pti Created
  ----------------------------------------------------------------------*/

  define input parameter piNumIncrement as integer no-undo. 

  define variable iQueriesServed as integer no-undo.

  /* Number of queries served */
  iQueriesServed = integer(getRegistry("DataDigger", "QueriesServed" )).
  if iQueriesServed = ? then iQueriesServed = 0.
  iQueriesServed = iQueriesServed + piNumIncrement.
  setRegistry("DataDigger", "QueriesServed", string(iQueriesServed) ).

  run setCounter(iQueriesServed).

END PROCEDURE. /* incQueriesServed */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObjects C-Win 
PROCEDURE initializeObjects :
/*------------------------------------------------------------------------
  Name         : initializeObject
  Description  : Initialize all kind of things, like restoring position 
                 and size from ini file. 
  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/

  define variable cDatabases    as character   no-undo.
  define variable cSetting      as character   no-undo.
  define variable iSetting      as integer     no-undo.
  define variable iValue        as integer     no-undo.
  define variable iField        as integer     no-undo.
  define variable cProgDir      as character   no-undo.
  define variable hColumn       as handle      no-undo.
  define variable cEnvironment  as character   no-undo.
  define variable cFilterFields as character no-undo. 
  define variable cButtons      as character no-undo. 
  define variable hBrowse       as handle    no-undo. 
  /* wfk - 5/15/2012 */
  c-win:y = 200.
  /* Find out where DataDigger is installed and how we're logged on */
  cProgDir = getTempDir().

  /* If the general ini file does not exist, create it */
  if search(cProgDir + 'DataDigger.ini') = ? then
  do:
    output to value(cProgDir + 'DataDigger.ini').
    output close. 
  end.
  load 'DataDigger' dir cProgDir base-key 'ini' no-error.
  if error-status:error then
    load 'DataDigger' dir cProgDir new base-key 'ini' no-error.


  /* Same for the helpfile (though it SHOULD exist!) */
  if search(cProgDir + 'DataDiggerHelp.ini') = ? then
  do:
    output to value(cProgDir + 'DataDiggerHelp.ini').
    output close. 
  end.
  load 'DataDiggerHelp' dir cProgDir base-key 'ini' no-error.
  if error-status:error then
    load 'DataDiggerHelp' dir cProgDir new base-key 'ini' no-error.

  /* Load or create personalized ini file */
  cEnvironment = substitute('DataDigger-&1', getUserName() ).

  /* If not exist, create it */
  if search(cProgDir + cEnvironment + '.ini') = ? then
  do:
    output to value(cProgDir + cEnvironment + '.ini').
    output close. 
  end.
  load cEnvironment dir cProgDir base-key 'ini' no-error.
  if error-status:error then
    load cEnvironment dir cProgDir new base-key 'ini' no-error.

  /* Set icon */
  C-Win:load-icon(getImagePath('DataDigger.ico')). 
/*   C-Win:context-help-file = cProgDir + 'DataDigger.hlp'. */

  do with frame frWhere:
    btnInsert:load-image          (getImagePath('Add.gif')).
  end.

  do with frame {&frame-name}:

    /* > UI Stuff */
    /* Show or hide toggle box for Debug mode */
/*     tgDebugMode:checked = glDebugMode. */
/*     tgDebugMode:hidden  = &IF DEFINED (UIB_is_running) &THEN no. &ELSE yes. &ENDIF */

    /* Load images for buttons */
    do with frame frSettings:
      btnSettings:load-image      (getImagePath('Settings.gif')).
      btnFavorites:load-image     (getImagePath('Favourites.gif')).
      btnDump-2:load-image        (getImagePath('Download.gif')).
      btnDump-2:load-image-insensitive (getImagePath('Download_Ins.gif')).
      btnLoad-2:load-image        (getImagePath('Upload.gif')).
      btnLoad-2:load-image-insensitive (getImagePath('Upload_Ins.gif')).
      btnQueries-3:load-image     (getImagePath('SavedQueries.gif')).
      btnChangeLog:load-image     (getImagePath('ReleaseNotes.gif')).
      btnAbout:load-image         (getImagePath('About.gif')).
    end.

    btnDataDigger:load-image      (getImagePath('DataDigger24x24.gif')).
    btnTools:load-image           (getImagePath('Tools.gif')).
    btnHelp:load-image            (getImagePath('Help.gif')).
    btnQueryTester:load-image     (getImagePath('QTester.gif')).

    btnTableFilter:load-image     (getImagePath('Filter.gif')).
    btnClearTableFilter:load-image(getImagePath('Clear.gif')).

    btnFieldFilter:load-image     (getImagePath('Filter.gif')).
    btnClearFieldFilter:load-image(getImagePath('Clear.gif')).

    btnIndexFilter:load-image     (getImagePath('Filter.gif')).
    btnClearIndexFilter:load-image(getImagePath('Clear.gif')).

    btnDataFilter:load-image      (getImagePath('Filter.gif')).
    btnClearDataFilter:load-image (getImagePath('Clear.gif')).

    btnViewData:load-image        (getImagePath('Execute.gif')).
    btnClear:load-image           (getImagePath('Clear.gif')).
    btnPrevQuery:load-image       (getImagePath('PrevQuery.gif')). 
    btnQueries:load-image         (getImagePath('SavedQueries.gif')).
    btnNextQuery:load-image       (getImagePath('NextQuery.gif')). 
    btnClipboard:load-image       (getImagePath('Clipboard.gif')).

    /* Same buttons on editor frame */
    btnViewData-2:load-image      (getImagePath('Execute.gif')).
    btnClear-2:load-image         (getImagePath('Clear.gif')).
    btnPrevQuery-2:load-image     (getImagePath('PrevQuery.gif')). 
    btnQueries-2:load-image       (getImagePath('SavedQueries.gif')).
    btnNextQuery-2:load-image     (getImagePath('NextQuery.gif')). 
    btnClipboard-2:load-image     (getImagePath('Clipboard.gif')).

    btnMoveTop:load-image         (getImagePath('First.gif')).
    btnMoveUp:load-image          (getImagePath('Back.gif')).
    btnReset:load-image           (getImagePath('Reset.gif')). 
    btnMoveDown:load-image        (getImagePath('Forward.gif')). 
    btnMoveBottom:load-image      (getImagePath('Last.gif')).
    btnAddFilter:load-image       (getImagePath('AddFilter.gif')).
    btnEditFilter:load-image      (getImagePath('FilterEdit.gif')).
    btnRemoveFilter:load-image    (getImagePath('RemoveFilter.gif')).
    btnAddFilter2:load-image      (getImagePath('AddFilter.gif')).
    btnEditFilter2:load-image     (getImagePath('FilterEdit.gif')).
    btnRemoveFilter2:load-image   (getImagePath('RemoveFilter.gif')).

    btnMoveUp:move-to-top().
    btnMoveDown:move-to-top().
    btnReset:move-to-top().
    btnMoveTop:move-to-top().
    btnMoveBottom:move-to-top().
    btnAddFilter:move-to-top().    
    btnEditFilter:move-to-top().    
    btnRemoveFilter:move-to-top().
    btnAddFilter2:move-to-top().    
    btnEditFilter2:move-to-top().    
    btnRemoveFilter2:move-to-top().

    /* Handle to the browse with fields of a file */
    ghFieldBrowse = brFields:handle in frame {&frame-name}.

    /* Set minimum size of the window */
    C-Win:min-width-pixels  = 650.
    C-Win:min-height-pixels = 430. /* 530 */
    /* to avoid scrollbars on the frame */
    frame {&frame-name}:scrollable = false.

    /* Additional tooltips */
    ficValue     :tooltip = ficValue   :tooltip + '~n~nCTRL-ENTER to execute'.
    ficWhere     :tooltip = ficWhere   :tooltip + '~n~nCTRL-ENTER to execute'.

    brFields     :tooltip = brFields   :tooltip + '~n~nRIGHT-CLICK to insert field+value'.
    brFields     :tooltip = brFields   :tooltip + '~nCTRL-RIGHT-CLICK to insert field'.
    brFields     :tooltip = brFields   :tooltip + '~nCTRL-ENTER to execute'.

    fiTableFilter:tooltip = fiTableFilter:tooltip + '~n~npress CTRL-ENTER to execute'.

    /* Create digits for the counter */
    run createCounter.

    /* restore width of table browser columns */
    do iField = 1 to brTables:num-columns:
      hColumn = brTables:get-browse-column(iField):handle.

      /* Get the width from registry */
      iValue = integer(getRegistry('DataDigger', substitute('ColumnWidth:&1', hColumn:name))) no-error.
      if iValue = ? then
      do:
        case hColumn:name:
          when 'cTableName'  then iValue = 110.
          when 'cDatabase'   then iValue =  50.
          when 'iNumQueries' then iValue =  30.
          when 'tLastUsed'   then iValue = 103.
        end case.
      end.
      if iValue <> ? then hColumn:width-pixels = iValue.

      on 'end-resize' of hColumn persistent run resizeFilters in this-procedure (input 0). 
    end.

    /* restore width of index browser columns */
    do iField = 1 to brIndexes:num-columns:
      hColumn = brIndexes:get-browse-column(iField):handle.

      /* Get the width from registry */
      iValue = integer(getRegistry('DataDigger', substitute('ColumnWidth:&1', hColumn:name))) no-error.
      if iValue = ? then 
      do:
        case hColumn:name:
          when 'cIndexName'   then iValue = 100.
          when 'cIndexFlags'  then iValue =  70.
          when 'cIndexFields' then iValue = 314.
        end case.
      end.
      if iValue <> ? then hColumn:width-pixels = iValue.

      on 'end-resize' of hColumn persistent run resizeFilters in this-procedure (input 2). 
    end.

    /* Build a list of all columns in the fieldbrowse.
     * We use this to hi-light fields that are in the prim index.
     */
    gcFieldBrowseColumnHandles = ''.
    gcFieldBrowseColumnNames   = ''.

    do iField = 1 to brFields:num-columns:
      hColumn = brFields:get-browse-column(iField):handle.
      gcFieldBrowseColumnHandles = gcFieldBrowseColumnHandles + ',' + string(hColumn).
      gcFieldBrowseColumnNames = gcFieldBrowseColumnNames + ',' + hColumn:name.

      /* Hide the cFormatOrg column */
      if hColumn:name = 'cFormatOrg' then hColumn:visible = false. 

      /* Get the width from registry */
      iValue = integer(getRegistry('DataDigger', substitute('ColumnWidth:&1', hColumn:name))) no-error.
      if iValue = ? then 
      do:
        case hColumn:name:
          when 'lShow'      then iValue =  15.
          when 'iOrder'     then iValue =  35.
          when 'cFieldName' then iValue = 150.
          when 'cDataType'  then iValue =  80.
          when 'cFormat'    then iValue =  75.
          when 'cLabel'     then iValue = 117. 
        end case.
      end.
      if iValue <> ? then hColumn:width-pixels = iValue.

      on 'end-resize' of hColumn persistent run resizeFilters in this-procedure (input 1). 
    end.
    gcFieldBrowseColumnHandles = trim(gcFieldBrowseColumnHandles,',').
    gcFieldBrowseColumnNames = trim(gcFieldBrowseColumnNames,',').


    /* Build a list of all columns in the index browse.
     * We use this to hi-light indexes that are inactive.
     */
    gcIndexBrowseColumnHandles = ''.

    do iField = 1 to brIndexes:num-columns:
      hColumn = brIndexes:get-browse-column(iField):handle.
      gcIndexBrowseColumnHandles = gcIndexBrowseColumnHandles + ',' + string(hColumn).
    end.
    gcIndexBrowseColumnHandles = trim(gcIndexBrowseColumnHandles,',').

    /* Restore active page */
    iValue = integer(getRegistry('DataDigger', 'ActivePage' )).
    if iValue = ? then iValue = 1.
    run setPage(iValue).
    /* Move index browse and associated filter fields to the left.
     * Just throw 'em on a stack, the resize event will take care of it.
     */
    fiIndexNameFilter  :x = tgSelAll:x.
    fiFlagsFilter      :x = tgSelAll:x.
    fiFieldsFilter     :x = tgSelAll:x.
    btnClearIndexFilter:x = tgSelAll:x.
    btnIndexFilter     :x = tgSelAll:x.
    btnAddFilter2      :x = tgSelAll:x.
    btnRemoveFilter2   :x = tgSelAll:x.

    /* Initialize the button panels to OFF */
    setUpdatePanel('no-record').

    /* Set the view type */
    cSetting = getRegistry('DataDigger', 'ViewType').
    if cSetting <> ? then run setViewType(cSetting).

    run resizeFilters (input 0).

    /* run setConnectionMenu. */
    run getTables(output table ttTable).
    /* < UI Stuff */

    /* 
     * > Restore 
     */

    /* Window position and size */
    iValue = integer(getRegistry('DataDigger', 'Window:x' )).
    if iValue = ? then iValue = 200.
    /* assign c-win:x = iValue no-error.

    /* Window has been parked at y=-1000 to get it out of sight */
    iValue = integer(getRegistry('DataDigger', 'Window:y' )).
    if iValue = ? then iValue = 200.
    if iValue <> ? then assign c-win:y = iValue no-error.

    iValue = integer(getRegistry('DataDigger', 'Window:height' )).
    if iValue = ? or iValue = 0 then iValue = 600.
    assign c-win:height-pixels = iValue no-error.

    iValue = integer(getRegistry('DataDigger', 'Window:width' )).
    if iValue = ? or iValue = 0 then iValue = 800.
    assign c-win:width-pixels = iValue no-error.
       */
    /* Number of queries served */
    run incQueriesServed(0).

    /* Get all connected databases */
    cDatabases = getDatabaseList().
    cbDatabaseFilter:list-items = ',' + cDatabases.

    /* Get sort for fields */
    cSetting = getRegistry('DataDigger','ColumnSortFields').
    if cSetting <> ? then
      brFields:set-sort-arrow(integer(entry(1,cSetting)), logical(entry(2,cSetting)) ).

    /* Get sort for indexes */
    cSetting = getRegistry('DataDigger','ColumnSortIndexes').
    if cSetting <> ? then
      brIndexes:set-sort-arrow(integer(entry(1,cSetting)), logical(entry(2,cSetting)) ).

    /* Show or hide hidden tables */
    cSetting = getRegistry('DataDigger', 'ShowHiddenTables').
    /* if cSetting <> ? then tHiddenTables:checked = logical(cSetting). */
    if cSetting <> ? then menu-item m_Show_hidden_tables:checked in menu POPUP-MENU-brTables = logical(cSetting).

    /* Get last used database from registry */        
    cSetting = getRegistry('DataDigger','Database').    
    if cSetting = '<empty>' then cSetting = ''.

    /* Restore last used database, if possible */
    if lookup(cSetting,cbDatabaseFilter:list-items) > 0 then    
      cbDatabaseFilter:screen-value = cSetting.                 
    else                                              
      cbDatabaseFilter:screen-value = cbDatabaseFilter:entry(1).          

    /* Hide or view the query editor */
    cSetting = getRegistry('DataDigger', 'QueryEditorState').
    if cSetting = ? then cSetting = 'Hidden'.
    setQueryEditor(cSetting).

    /* Get sort for fields */
    cSetting = getRegistry('DataDigger','ColumnSortTables').
    if cSetting <> ? then
      brTables:set-sort-arrow(integer(entry(1,cSetting)), logical(entry(2,cSetting)) ).

    /* Take whatever is now selected in the db dropdown */
    cSetting = getCurrentDatabase().
    run setDbContext(input (if cbDatabaseFilter:screen-value = ? then '<empty>' else cbDatabaseFilter:screen-value)).

    /* < Restore  */

    /* <BEU> */
    define variable cQueryTesterPath as character no-undo.

    cQueryTesterPath = getRegistry
      ( 'QueryTester'
      , 'path' 
      ).

    file-info:file-name = search(cQueryTesterPath).
    if file-info:full-pathname = ? then
      btnQueryTester:hidden = true.
    /* </BEU> */

  end. /* do with frame */

  apply 'window-resized' to c-win.
  apply 'value-changed' to brTables.
end procedure. /* initializeObjects */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadData C-Win 
PROCEDURE loadData :
/*------------------------------------------------------------------------
  Name         : loadData 
  Description  : Load records from a dumpfile 
  ---------------------------------------------------------------------- 
  18-03-2009 pti Created
  ----------------------------------------------------------------------*/

  run value(getProgramDir() + 'wLoadData.w')
    ( input getCurrentDatabase()
    , input getCurrentTable()
    , input ''
    ).

end procedure. /* loadData */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE masquer C-Win 
PROCEDURE masquer :
DEFINE INPUT  PARAMETER hColumn AS HANDLE     NO-UNDO.
DEFINE INPUT  PARAMETER hMenuTri AS HANDLE     NO-UNDO.
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
hColumn:VISIBLE = FALSE NO-ERROR.
hMenuTri:VISIBLE = FALSE NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE menuDropDataBrowse C-Win 
PROCEDURE menuDropDataBrowse :
/*------------------------------------------------------------------------
  Name         : menuDropDataBrowse 
  Description  : Enable / disable items in the context menu
  ---------------------------------------------------------------------- 
  18-03-2009 pti Created
  ----------------------------------------------------------------------*/

  define variable hMenuItem      as handle      no-undo.
  define variable cColumnName    as character   no-undo.
  define variable cColumnValue   as character   no-undo.
  define variable cColumnClicked as character   no-undo. 
  define variable lOk            as logical     no-undo. 
  define variable lColumnsHidden as logical     no-undo. 
  define variable iColumn        as integer     no-undo. 
  define variable hColumn        as handle      no-undo. 

  /* Select the row we clicked on */
  run selectClickedRow(ghDataBrowse, output lOk, output cColumnClicked).
  if not lOk then return. 

  if num-entries( ghDataBrowse:private-data,chr(1) ) = 3 then
    assign
      cColumnName  = entry(1, ghDataBrowse:private-data,chr(1))
      cColumnValue = entry(2, ghDataBrowse:private-data,chr(1))
      .

  /* If there are hidden columns, enable the menu-item 'unhide' */
  lColumnsHidden = can-find(first ttField where ttField.lDataField = true and ttField.lShow = false).
/*   findHiddenColumns:                                   */
/*   do iColumn = 1 to ghDataBrowse:num-columns:          */
/*     hColumn = ghDataBrowse:get-browse-column(iColumn). */
/*     if hColumn:visible = no then do:                   */
/*       lColumnsHidden = yes.                            */
/*       leave findHiddenColumns.                         */
/*     end.                                               */
/*   end.                                                 */

  /* Enable/disable all current items */
  hMenuItem = ghDataBrowse:popup-menu:first-child.

  do while valid-handle(hMenuItem):
    if hMenuItem:subtype = 'normal' then 
    do:
      hMenuItem:label = hMenuItem:private-data.

      /* If we did not use right mouse click but shift-f10 then
       * we do not know the column name. In that case disable all 
       * menu items that do something with the column value
       */
      if cColumnClicked = '' and lookup(hMenuItem:name,'edit,view,dump,delete') = 0 then
        hMenuItem:sensitive = false.
      else
        hMenuItem:sensitive = true.

      /* Entry 'Unhide Columns' is only enabled when there is at least 1 hidden column */
      if hMenuItem:name = 'unhideColumn' then 
        hMenuItem:sensitive = lColumnsHidden.
    end.

    if ghDataBrowse:query:num-results = 0 then hMenuItem:sensitive = no.

    hMenuItem = hMenuItem:next-sibling.
  end.

end procedure. /* menuDropDataBrowse */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveField C-Win 
PROCEDURE moveField :
/*------------------------------------------------------------------------
  Name         : moveField
  Description  : Move a field up or down in the field browse. 

  ----------------------------------------------------------------------
  02-12-2009 pti Created
  ----------------------------------------------------------------------*/

  define input parameter pcDirection as character no-undo. 

  define variable iTemp          as integer   no-undo.
  define variable iCurrentOrder  as integer   no-undo.
  define variable rCurrentRecord as rowid     no-undo. 
  define variable iCurrentRow    as integer   no-undo. 
  define variable hBuffer        as handle    no-undo. 
  define variable hQuery         as handle    no-undo. 
  define variable cFieldOrder    as character no-undo.

  define buffer ttFieldOrg for ttField.
  define buffer ttField    for ttField.

  /* Remember where we are in the browse */
  rCurrentRecord = browse brFields:query:get-buffer-handle(1):rowid.
  iCurrentRow    = browse brFields:focused-row.

  case pcDirection:
    when 'top'    then iCurrentRow = 1.
    when 'up'     then if iCurrentRow > 1 then iCurrentRow = iCurrentRow - 1.
    when 'down'   then if iCurrentRow < 10 then iCurrentRow = iCurrentRow + 1.
    when 'bottom' then iCurrentRow = 10.
  end case.

  .setWindowFreeze(yes).

  /* Find the active record */
  find ttFieldOrg where rowid(ttFieldOrg) = browse brFields:query:get-buffer-handle(1):rowid no-error.
  if not available ttFieldOrg then return. 

  case pcDirection:
    when 'top'    then ttFieldOrg.iOrder = -1.
    when 'up'     then ttFieldOrg.iOrder = ttFieldOrg.iOrder - 1.5.
    when 'down'   then ttFieldOrg.iOrder = ttFieldOrg.iOrder + 1.5.
    when 'bottom' then ttFieldOrg.iOrder = 999999999.
  end case.

  iCurrentOrder = 0.
  repeat preselect each ttFieldOrg by ttFieldOrg.iOrder:
    find next ttFieldOrg.
    assign 
      iCurrentOrder    = iCurrentOrder + 1
      ttFieldOrg.iOrder = iCurrentOrder
      cFieldOrder      = cFieldOrder + ',' + ttFieldOrg.cFieldName.
  end.
  cFieldOrder = trim(cFieldOrder,',').

  /* Save changed order of the field. If it is blank, it will be deleted from registry */
  setRegistry( substitute('DB:&1',getCurrentDatabase())
             , substitute('&1:fieldOrder', getCurrentTable() )
             , if cFieldOrder <> getFieldList('iOrderOrg') then cFieldOrder else ?
             ).

  /* And resort it */
  run reopenFieldBrowse('iOrder', yes).

  /* Create a query, similar to what is used in the browse and point it to the active record */

  /* Reopen browse */
  browse brFields:set-repositioned-row(iCurrentRow,"ALWAYS").
  browse brFields:query:reposition-to-rowid(rCurrentRecord).

  setWindowFreeze(no).

end procedure. /* moveField */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopenDataBrowse C-Win 
PROCEDURE reopenDataBrowse :
/*------------------------------------------------------------------------
  Name         : reopenDataBrowse
  Description  : Build the query, based on where-box and filter fields

  ----------------------------------------------------------------------
  16-01-2009 pti Created
  04-02-2011 pti Restructured, merged with some other procedures.
  ----------------------------------------------------------------------*/

  define input parameter pcSortField  as character   no-undo.
  define input parameter plAscending  as logical     no-undo.

  define variable cColumnName     as character   no-undo.
  define variable cDatabase       as character   no-undo.
  define variable cQuery          as character   no-undo.
  define variable cTable          as character   no-undo.
  define variable hBrowseBuffer   as handle      no-undo.
  define variable hBrowseColumn   as handle      no-undo.
  define variable hField          as handle      no-undo.
  define variable hFilterField    as handle      no-undo.
  define variable hQuery          as handle      no-undo.
  define variable iColumn         as integer     no-undo.
  define variable iMaxColumns     as integer     no-undo.
  define variable iNumRecords     as integer     no-undo.
  define variable iStartTime      as integer     no-undo.
  define variable lPrepare        as logical     no-undo.

  define variable cOldSort       as character    no-undo.
  define variable cBaseQuery     as character    no-undo.
  define variable lAscending     as logical      no-undo.
  define variable iWord          as integer      no-undo.

  define buffer ttField for ttField. 
  define buffer bField for ttField. 

  /* Increase query counter */
  cDatabase = gcCurrentDatabase.
  cTable = gcCurrentTable.

  run incQueriesOfTable(cDatabase, cTable, +1).
  run incQueriesServed(+1).

  /* Freeze! */
  session:set-wait-state('general'). 
  setWindowFreeze(yes).

  /* Clean up existing dynamic stuff */
  if valid-handle(ghDataBrowse) then
  do:
    /* Find out what the current sort is */
    cBaseQuery = ghDataBrowse:query:prepare-string.
    lAscending = (lookup('DESCENDING',cBaseQuery,' ') = 0).

    do iWord = 1 to num-entries(cBaseQuery,' ') - 1:
      if entry(iWord,cBaseQuery,' ') = 'BY' then
        cOldSort = entry(iWord + 1,cBaseQuery,' ').
    end.

    if pcSortField = cOldSort then
      lAscending = not lAscending.
    else 
      lAscending = true.

    /* Sort direction might be overruled */
    if plAscending <> ? then lAscending = plAscending.

    /* Clean up */
    run deleteDataFilters.
    delete object ghDataBrowse:query:get-buffer-handle(1) no-error.
    delete object ghDataBrowse:query no-error.
    delete object ghDataBrowse no-error.
  end.

  /* Que? WTF is this for? DEBUG! */
  if not valid-handle(ghFieldBrowse) then 
    run collectFieldInfo(input self:screen-value).

  /* Get the selected fields to display in the browse */        
  giQueryPointer = 1.

  /* Create the query */
  create buffer hBrowseBuffer for table cDatabase + '.' + cTable.
  create query hQuery.
  hQuery:set-buffers(hBrowseBuffer).
  cQuery = getDataQuery().

  /* Sort field might be overruled when user clicks on data column header 
   * When query is: FOR EACH CUSTOMER BY CUSTNUM but we click on the ADDRESS 
   * column, the query should be rewritten to FOR EACH CUSTOMER BY ADDRESS
   */
  if pcSortField <> ? and pcSortField <> "" then
  do:
    if lookup('BY',cQuery,' ') > 0 then
      cQuery = substitute('&1 BY &2 &3 INDEXED-REPOSITION'
                         , trim(substring(cQuery,1,index(cQuery,' BY ')))
                         , pcSortField 
                         , string(lAscending,'/DESCENDING')
                         ).
    else
      cQuery = substitute('&1 BY &2 &3 INDEXED-REPOSITION'
                         , trim(substring(cQuery,1,index(cQuery,' INDEXED-REPOSITION')))
                         , pcSortField 
                         , string(lAscending,'/DESCENDING')
                         ).
  end.

  /* If the user has set a sort field, use that to set the sort arrow */
  iWord = lookup('BY',cQuery,' ').
  if iWord > 0 then 
    assign pcSortField = entry(iWord + 1,cQuery,' ')
           lAscending  = (lookup('DESCENDING',cQuery,' ') = 0).

  /* for DWP query tester */
  publish 'message' (input 1, "cQuery = " + cQuery ).

  /* Try to open it */
  lPrepare = hQuery:query-prepare(cQuery) no-error.

  /* if the QUERY-PREPARE failed because of the where-clause, reopen it without it */ 
  if not lPrepare then 
  do with frame {&frame-name}:
    ficWhere:bgcolor = 12. /* red */
    ficWhere:fgcolor = 14. /* yellow */
    fiIndexInfo:screen-value = "".
    ficWhere:tooltip = substitute('Open query failed due to this error:~n~n&1~n~nYour WHERE-clause will be ignored.'
                                 , trim(error-status:get-message(1))
                                 ).
    cQuery = substitute("for each &1.&2 no-lock", cDatabase, cTable).
    lPrepare = hQuery:query-prepare(cQuery).
    publish 'query' (input cQuery).
    apply "entry" to ficWhere. 
  end.
  else
  do with frame {&frame-name}:
    ficWhere:bgcolor = ?. /* default */
    ficWhere:fgcolor = ?. /* default */
    fiIndexInfo:screen-value = substitute('Index used: &1', hQuery:index-information(1) ). /* only show index name */
    ficWhere:tooltip = getReadableQuery(cQuery).
  end.


  /* Try to grab as many records as we can in 500 msec. This will give
   * an indication of the amount of records in the table 
   */
  hQuery:query-open().
  iStartTime = etime.
  hQuery:get-first.
  do while etime - iStartTime < 500 and not hQuery:query-off-end:
    hQuery:get-next.
    iNumRecords = iNumRecords + 1.
  end.

  /* Show nr of records */
  setNumRecords(iNumRecords, hQuery:query-off-end, etime - iStartTime).

  /* Set the query back to the first record */
  hQuery:get-first.

  /* Start building */
  create browse ghDataBrowse
    assign
    name              = 'brData'
    frame             = frame {&frame-name}:handle
    multiple          = true
    query             = hQuery  
    x                 = rctData:x + 5
    y                 = rctData:y + 5 + 21 /* extra space for filters */
    width-pixels      = rctData:width-pixels - 10
    height-pixels     = rctData:height-pixels - 10 - 23 /* extra space for filters */
    row-marker        = true
    separators        = true
    read-only         = false
    sensitive         = true
    visible           = false
    no-validate       = true
    column-resizable  = true
    column-scrolling  = true /* scroll with whole columns at a time */
    context-help-id   = 100
    triggers:
      on 'CTRL-A'           persistent run dataSelectAll       in this-procedure (ghDataBrowse).
      on 'CTRL-D'           persistent run dataSelectNone      in this-procedure (ghDataBrowse).
      on 'ROW-LEAVE'        persistent run dataRowLeave        in this-procedure. 
      on 'ROW-DISPLAY'      persistent run dataRowDisplay      in this-procedure (hBrowseBuffer).
      on 'START-SEARCH'     persistent run dataColumnSort      in this-procedure.
      on 'insert-mode'      persistent run applyChoose         in this-procedure (btnAdd:handle).
      on 'delete-character' persistent run applyChoose         in this-procedure (btnDelete:handle).
      on 'value-changed'    persistent run dataRowValueChanged in this-procedure (hBrowseBuffer).
      on 'end'              persistent run dataRowJumpToEnd    in this-procedure (hBrowseBuffer).
      on 'scroll-notify'    persistent run dataScrollNotify    in this-procedure (ghDataBrowse). 
      on 'default-action'   persistent run dataDoubleClick     in this-procedure (ghDataBrowse). 
      on 'off-home'         persistent run dataOffHome         in this-procedure. 
    end triggers.

  /* Create the context menu for the databrowse */
  run createMenuDataBrowse.

  /* Add the columns to the browse */
  gcDataBrowseColumns = ''.
  gcDataBrowseColumnNames = ''.

  /* Maximum number of columns */
  iMaxColumns = integer(getRegistry("DataDigger", "MaxColumns" )).
  if iMaxColumns = ? then iMaxColumns = 500.

  iColumn = 0.

  addColumnLoop:
  for each ttField where ttField.lDataField = true by ttField.iOrder:

    /* If it is an extent field, find the template and see if that one is selected */
    if ttField.iExtent > 0 and 
      not can-find(first bField where bField.cFieldName = ttField.cFieldName
                                  and bField.lMetaField = true
                                  and bField.lShow      = true) then next. 

    /* Protect against too much columns. This gives error:
     * SYSTEM ERROR: stkpush: stack overflow. Increase the -s parameter. (279)
     */
    iColumn = iColumn + 1.
    if iColumn > iMaxColumns then leave addColumnLoop.

    /* Recid and Rowid column */
    if ttField.lMetaField 
      and can-do('RECID,ROWID', ttField.cFieldName) then
    do:
      if ttField.lShow then
        ttField.hColumn = ghDataBrowse:add-calc-column(ttField.cDataType, ttField.cFormat, '', ttField.cFullName).

      next addColumnLoop.
    end.

    /* Get handle to this field in the buffer */
    hField = hBrowseBuffer:buffer-field(ttField.cFieldName):handle.

    /* Adjust format in the buffer */
    if not (    ttField.cDataType begins 'int' /* use BEGINS to cover integer / int64 and extents of both */
            and ttField.cFormat begins 'hh:' ) then
    do:
      hField:format = ttField.cFormat no-error.

      if error-status:error then
      do:
        /* If there is something wrong the the format, just reset it to the original format */
        run showHelp('FormatError', ttField.cFormat + ',' + ttField.cFormatOrg).
        ttField.cFormat = ttField.cFormatOrg.

        /* Delete wrong format from ini file */
        setRegistry( substitute('DB:&1',cDatabase)
                   , substitute('&1.&2:format',cTable,ttField.cFieldName)
                   , ?
                   ).

        browse brFields:refresh().
        hField:format = ttField.cFormat no-error.
      end.
    end. /* not an integer-time-field */

    /* Add a calculated column for integer fields that would like to be a time field */
    cColumnName = substitute('&1.&2.&3', cDatabase, cTable, ttField.cFullName).
    if    ttField.cDataType begins 'int' /* use BEGINS to cover integer / int64 and extents of both */
      and ttField.cFormat begins 'hh:' then 
      ttField.hColumn = ghDataBrowse:add-calc-column('character','x(8)','', cColumnName ) no-error.
    else
      ttField.hColumn = ghDataBrowse:add-like-column(cColumnName).

    /* Set label and name */
    ttField.hColumn:label     = ttField.cFullName.
    ttField.hColumn:name      = ttField.cFullName.
    ttField.hColumn:read-only = yes.
    ttField.hColumn:visible   = ttField.lShow.

    /* If we're sorting on this column, set sort arrow */
    if ttField.hColumn:name = pcSortField then 
      ghDataBrowse:set-sort-arrow(iColumn, lAscending ).

    on 'entry'      of ttField.hColumn persistent run dataColumnEntry  in this-procedure. 
    on 'leave'      of ttField.hColumn persistent run dataColumnLeave  in this-procedure. 
    on 'end-resize' of ttField.hColumn persistent run dataScrollNotify in this-procedure (ghDataBrowse).

    /* Build list of column handles for the rowDisplay trigger */
    assign 
      gcDataBrowseColumns     = gcDataBrowseColumns     + "," + string(ttField.hColumn)
      gcDataBrowseColumnNames = gcDataBrowseColumnNames + "," + ttField.hColumn:name
      .

    /* Create a filterfield for this column */
    create fill-in hFilterField
      assign
        frame         = ghDataBrowse:frame
        name          = 'filter_' + ttField.cFullName
        x             = ghDataBrowse:x
        y             = rctData:y + 5 + 21 - 23 /* Extra space for filters */
        width-pixels  = 10
        height-pixels = 21
        sensitive     = true
        visible       = false
        format        = 'x(40)'
        private-data  = ttField.cFullName
        screen-value  = (if ttField.cFilterValue <> '' then ttField.cFilterValue else ttField.cFullName)
      triggers:
        on 'entry'         persistent run filterFieldEntry        in this-procedure (hFilterField).
        on 'leave'         persistent run filterFieldLeave        in this-procedure (hFilterField).
        on 'any-printable' persistent run filterFieldAnyPrintable in this-procedure (hFilterField).
        on 'value-changed' persistent run filterFieldValueChanged in this-procedure (hFilterField).
        on 'shift-del'     persistent run applyEvent              in this-procedure (btnClearDataFilter:handle,'choose').
        on 'return'        persistent run applyEvent              in this-procedure (btnDataFilter:handle,'choose').
        on 'cursor-down'   persistent run filterFieldCursorDown   in this-procedure (hFilterField, ttField.hColumn).
      end triggers.

    assign ttField.hFilter = hFilterField.
    setFilterFieldColor(hFilterField).

  end. /* for each ttField */

  gcDataBrowseColumns     = trim(gcDataBrowseColumns,",").
  gcDataBrowseColumnNames = trim(gcDataBrowseColumnNames,",").

  /* Show warning when too much columns */
  if ghDataBrowse:num-columns >= iMaxColumns then
  do:
    run showHelp('TooManyColumns', iMaxColumns).

    if getRegistry('DataDigger:help', 'TooManyColumns:answer') = '1' then
      apply 'choose' to btnSettings in frame frSettings.
  end.
  else 
    fiWarning:visible = no.

  /* Show the browse */
  ghDataBrowse:visible = true.

  /* Limit fields to a max of 300px wide 
   * This must be done after the browse is realized
   */
  getFilterLoop:
  do iColumn = 1 to ghDataBrowse:num-columns:
    hBrowseColumn = ghDataBrowse:get-browse-column(iColumn).
    hBrowseColumn:width-pixels = minimum(300, hBrowseColumn:width-pixels).
  end.

  /* Adjust all filters */
  run dataScrollNotify(ghDataBrowse).

  /* Activate buttons */
  setUpdatePanel('display').

  /* For some reasons, these #*$&# scrollbars keep coming back */
  run showScrollBars(frame {&frame-name}:handle, no, no). /* KILL KILL KILL */

  /* Unfreeze it */
  session:set-wait-state(''). 
  setWindowFreeze(no).

  apply 'entry' to ghDataBrowse. 

end procedure. /* reopenDataBrowse */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopenFieldBrowse C-Win 
PROCEDURE reopenFieldBrowse :
/*------------------------------------------------------------------------
  Name         : reopenFieldBrowse
  Description  : Open the field browse again, taking into account the 
                 filter values the user has entered. 

  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/

  define input parameter pcSortField  as character   no-undo.
  define input parameter plAscending  as logical     no-undo.

  define variable hColumn          as handle      no-undo.
  define variable hBuffer          as handle      no-undo.
  define variable hQuery           as handle      no-undo.
  define variable iColumn          as integer     no-undo.
  define variable lAscending       as logical     no-undo.
  define variable cOldSort         as character   no-undo. 
  define variable cNewSort         as character   no-undo. 
  define variable cOrderFilter     as character   no-undo. 
  define variable cNameFilter      as character   no-undo. 
  define variable cTypeFilter      as character   no-undo. 
  define variable cFormatFilter    as character   no-undo. 
  define variable cLabelFilter     as character   no-undo. 
  define variable cQuery           as character   no-undo.
  define variable rCurrentRecord   as rowid       no-undo. 
  define variable lFieldsFound     as logical    no-undo.

  /* Get filters */
  do with frame {&frame-name}:
    cOrderFilter  = (if fiOrderFilter:screen-value = fiOrderFilter:private-data then '' else fiOrderFilter:screen-value ).
    cNameFilter   = getMatchesValue(fiNameFilter  :handle).      
    cTypeFilter   = getMatchesValue(fiTypeFilter  :handle).      
    cFormatFilter = getMatchesValue(fiFormatFilter:handle).    
    cLabelFilter  = getMatchesValue(fiLabelFilter :handle).
  end.

  run setRedLines.

  /* Protect routine against invalid input */
  if pcSortField = '' then pcSortField = ?.

  /* Remember record we're on */
  if brFields:num-selected-rows > 0 then 
    rCurrentRecord = brFields:query:get-buffer-handle(1):rowid.

  /* Find out what the current sort is */
  run getColumnSort(input brFields:handle, output cOldSort, output lAscending).

  /* If no new sortfield is provided, we don't want to change the sort.
   * This happens when we press the filter button.
   */
  if pcSortField = ? then
    assign 
      cNewSort   = cOldSort
      lAscending = lAscending. /* dont change order */
  else
  if pcSortField = cOldSort then
    assign 
      cNewSort   = cOldSort
      lAscending = not lAscending. /* invert order */
  else
    /* New field */
    assign 
      cNewSort   = pcSortField
      lAscending = true.

  /* Sort direction might be overruled */
  if plAscending <> ? then lAscending = plAscending.

  /* Wich column should have what arrow? */
  run setSortArrow(brFields:handle, cNewSort, lAscending).

  /* If - and only if - the sort is on 'Order', the buttons for moving are enabled */
  btnMoveUp:sensitive     = (cNewSort = "iOrder").
  btnMoveDown:sensitive   = (cNewSort = "iOrder").
  btnMoveTop:sensitive    = (cNewSort = "iOrder").
  btnMoveBottom:sensitive = (cNewSort = "iOrder").

  /* Close open query */
  if valid-handle(brFields:query) then brFields:query:query-close().

  /* Build the query */
  create query hQuery.
  create buffer hBuffer for table 'ttField'.
  hQuery:set-buffers(hBuffer).

  /* reset all 'visible' flags */
  for each ttField: assign ttField.lVisible = false. end.

  cQuery = 'for each ttField where ttField.lMetaField = true'.
  if cOrderFilter  <> "" and cOrderFilter  <> "*" then cQuery = substitute("&1 and ttField.iOrder      =       &2", cQuery, quoter(cOrderFilter )).
  if cNameFilter   <> "" and cNameFilter   <> "*" then cQuery = substitute("&1 and ttField.cFieldName  matches &2", cQuery, quoter(cNameFilter  )).
  if cTypeFilter   <> "" and cTypeFilter   <> "*" then cQuery = substitute("&1 and ttField.cDataType   matches &2", cQuery, quoter(cTypeFilter  )).
  if cFormatFilter <> "" and cFormatFilter <> "*" then cQuery = substitute("&1 and ttField.cFormat     matches &2", cQuery, quoter(cFormatFilter)).
  if cLabelFilter  <> "" and cLabelFilter  <> "*" then cQuery = substitute("&1 and ttField.cLabel      matches &2", cQuery, quoter(cLabelFilter )).
  cQuery = substitute("&1 by &2 &3", cQuery, cNewSort, string(lAscending,'/descending')).

  hQuery:query-prepare(cQuery).
  hQuery:query-open().

  /* Set 'visibility flags' for all visible fields */
  hQuery:get-first.
  repeat while not hQuery:query-off-end:
    hBuffer::lVisible = true.
    lFieldsFound = true.
    hQuery:get-next.
  end.

  hQuery:get-first.

  /* Attach query to the browse */
  brFields:query in frame {&frame-name} = hQuery.

  /* Jump back to selected row */
  if not hQuery:query-off-end 
    and can-find(ttField where rowid(ttField) = rCurrentRecord) then
  do:
    hQuery:reposition-to-rowid(rCurrentRecord) no-error.
    brFields:select-focused-row().
  end.

  /* If we have fields, set VIEW button on */
  btnViewData:sensitive     = lFieldsFound. 
  btnViewData-2:sensitive in frame frWhere = lFieldsFound. 
  btnWhere:sensitive        = lFieldsFound. 
  tgSelAll:sensitive        = lFieldsFound. 

  btnMoveUp:sensitive       = lFieldsFound.
  btnMoveDown:sensitive     = lFieldsFound.
  btnReset:sensitive        = lFieldsFound.
  btnMoveTop:sensitive      = lFieldsFound.
  btnMoveBottom:sensitive   = lFieldsFound.
  btnAddFilter:sensitive    = lFieldsFound.    
  btnRemoveFilter:sensitive = gcFieldFilterList <> "".

END PROCEDURE. /* reopenFieldBrowse */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopenIndexBrowse C-Win 
PROCEDURE reopenIndexBrowse :
/*------------------------------------------------------------------------
  Name         : reopenIndexBrowse
  Description  : Reopen the browse with indexes. 

  ----------------------------------------------------------------------
  01-09-2009 pti Created
  ----------------------------------------------------------------------*/

  define input parameter pcSortField  as character   no-undo.
  define input parameter plAscending  as logical     no-undo.

  define variable hColumn          as handle      no-undo.
  define variable hQuery           as handle      no-undo.
  define variable iColumn          as integer     no-undo.
  define variable lAscending       as logical     no-undo.
  define variable hBuffer          as handle      no-undo.
  define variable cOldSort         as character   no-undo. 
  define variable cNewSort         as character   no-undo. 
  define variable cNameFilter      as character   no-undo. 
  define variable cFlagFilter      as character   no-undo. 
  define variable cFieldsFilter    as character   no-undo. 
  define variable cQuery           as character   no-undo.
  define variable rCurrentRecord   as rowid       no-undo. 
  define variable lFieldsFound     as logical     no-undo.

  /* Set filters */
  do with frame {&frame-name}:
    cNameFilter   = getMatchesValue(fiIndexNameFilter:handle).
    cFlagFilter   = getMatchesValue(fiFlagsFilter    :handle).
    cFieldsFilter = getMatchesValue(fiFieldsFilter   :handle).
  end.

  run setRedLines.

  /* Protect routine against invalid input */
  if pcSortField = '' then pcSortField = ?.

  /* Find out what the current sort is */
  run getColumnSort(input brIndexes:handle, output cOldSort, output lAscending).

  /* If no new sortfield is provided, we don't want to change the sort.
   * This happens when we press the filter button.
   */
  if pcSortField = ? then
    assign 
      cNewSort   = cOldSort
      lAscending = lAscending. /* dont change order */
  else
  if pcSortField = cOldSort then
    assign 
      cNewSort   = cOldSort
      lAscending = not lAscending. /* invert order */
  else
    /* New field */
    assign 
      cNewSort   = pcSortField
      lAscending = true.

  /* Protection against first-time usage (in that case
   * sort is not set).
   */
  if cNewSort = "" then cNewSort = brIndexes:get-browse-column(1):name.

  /* Sort direction might be overruled */
  if plAscending <> ? then lAscending = plAscending.

  /* Wich column should have what arrow? */
  run setSortArrow(brIndexes:handle, cNewSort, lAscending).

  /* Remember record */
  if brIndexes:num-selected-rows > 0 then
    rCurrentRecord = brIndexes:query:get-buffer-handle(1):rowid.

  /* Build the query */
  create query hQuery.
  create buffer hBuffer for table 'ttIndex'.
  hQuery:set-buffers(hBuffer).

  cQuery = 'for each ttIndex where true'.
  if cNameFilter   <> "" and cNameFilter   <> "*" then cQuery = substitute("&1 and ttIndex.cIndexName   matches &2", cQuery, quoter(cNameFilter  )).
  if cFlagFilter   <> "" and cFlagFilter   <> "*" then cQuery = substitute("&1 and ttIndex.cIndexFlags  matches &2", cQuery, quoter(cFlagFilter  )).
  if cFieldsFilter <> "" and cFieldsFilter <> "*" then cQuery = substitute("&1 and ttIndex.cIndexFields matches &2", cQuery, quoter(cFieldsFilter)).
  cQuery = substitute("&1 by &2 &3", cQuery, cNewSort, string(lAscending,'/descending')).

  hQuery:query-prepare(cQuery).
  hQuery:query-open().
  hQuery:get-first().

  lFieldsFound = not hQuery:query-off-end.
  btnAddFilter2:sensitive    = lFieldsFound.    
  btnRemoveFilter2:sensitive = gcFieldFilterList <> "".

  /* Attach query to the browse */
  brIndexes:query in frame {&frame-name} = hQuery.

  /* Jump back to selected row */
  if not hQuery:query-off-end 
    and rCurrentRecord <> ? then
  do:
    hQuery:reposition-to-rowid(rCurrentRecord) no-error.
    brIndexes:select-focused-row().
  end.

END PROCEDURE. /* reopenIndexBrowse */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopenTableBrowse C-Win 
PROCEDURE reopenTableBrowse :
/*------------------------------------------------------------------------
  Name         : reopenTableBrowse
  Description  : Open the table browse again, taking into account the 
                 filter values the user has entered. 

  ----------------------------------------------------------------------
  29-10-2009 pti Created
  ----------------------------------------------------------------------*/

  define input parameter pcSortField  as character   no-undo.
  define input parameter plAscending  as logical     no-undo.
  define input parameter pcFieldName  as character   no-undo.

  define variable hColumn           as handle      no-undo.
  define variable hQuery            as handle      no-undo.
  define variable iColumn           as integer     no-undo.
  define variable iNumQueriesFilter as integer     no-undo.
  define variable lAscending        as logical     no-undo.
  define variable lShowHiddenTables as logical     no-undo.
  define variable hBuffer           as handle      no-undo.
  define variable cOldSort          as character   no-undo. 
  define variable cNewSort          as character   no-undo. 
  define variable cTableFilter      as character   no-undo. 
  define variable cQuery            as character   no-undo.
  define variable rCurrentRecord    as rowid       no-undo. 
  define variable cTableSet         as character   no-undo.
  define variable cDatabaseFilter   as character   no-undo.
  define variable tLastUsedFilter   as datetime    no-undo.

  do with frame {&frame-name}:

    /* Get filters */
    cTableFilter      = getMatchesValue(fiTableFilter:handle).
    cDatabaseFilter   = cbDatabaseFilter:screen-value.
    lShowHiddenTables = menu-item m_Show_hidden_tables:checked in menu POPUP-MENU-brTables. /* tHiddenTables:checked. */
    iNumQueriesFilter = integer(fiNumQueriesFilter:screen-value).
    tLastUsedFilter   = datetime(fiLastUsedFilter:screen-value) no-error.

    run setRedLines.

    /* Protect routine against invalid input */
    if pcSortField = '' then pcSortField = ?.
    if cDatabaseFilter = ? then cDatabaseFilter = '*'.

    /* Remember record */
    if brTables:num-selected-rows > 0 then
      rCurrentRecord = brTables:query:get-buffer-handle(1):rowid.

    /* Find out what the current sort is */
    run getColumnSort(input brTables:handle, output cOldSort, output lAscending).

    /* If no new sortfield is provided, we don't want to change the sort.
     * This happens when we press the filter button.
     */
    if pcSortField = ? then
      assign 
        cNewSort   = cOldSort
        lAscending = lAscending. /* dont change order */
    else
    if pcSortField = cOldSort then
      assign 
        cNewSort   = cOldSort
        lAscending = not lAscending. /* invert order */
    else
      /* New field */
      assign 
        cNewSort   = pcSortField
        lAscending = true.

    /* Sort direction might be overruled */
    if plAscending <> ? then lAscending = plAscending.

    /* Protection against wrong parameters (in case of first-time usage sort is not set). */
    if cNewSort = "" or cNewSort = ? then cNewSort = brTables:get-browse-column(1):name.

    /* Wich column should have what arrow? */
    run setSortArrow(brTables:handle, cNewSort, lAscending).


    /* Special case: sort on datetime. Sorting descending will show the 
     * blank datetimes first. We don't want that */
    if cNewSort = "tLastUsed" then cNewSort = "tLastUsed <> ? descending by tLastUsed".


    /* Close open query */
    if valid-handle(brTables:query) then brTables:query:query-close().

    /* Build the query */
    create query hQuery.
    create buffer hBuffer for table 'ttTable'.
    hQuery:set-buffers(hBuffer).

    /* Build query */
    cQuery = substitute('for each ttTable where cDatabase matches &1', quoter(cDatabaseFilter)).
    cQuery = substitute("&1 and cTableName matches &2", cQuery, quoter(cTableFilter )).
    cQuery = substitute("&1 and iNumQueries >= &2", cQuery, iNumQueriesFilter ).
    if not lShowHiddenTables then cQuery = substitute("&1 and lHidden = no", cQuery ).
    if tLastUsedFilter <> ? then
      cQuery = substitute("&1 and tLastUsed >= &2", cQuery, quoter(tLastUsedFilter) ).
    cQuery = substitute("&1 by &2 &3", cQuery, cNewSort, string(lAscending,'/descending')).

    hQuery:query-prepare(cQuery).
    hQuery:query-open().

    /* Attach query to the browse */
    brTables:query in frame {&frame-name} = hQuery.

    /* Jump back to selected row */
    if not hQuery:query-off-end 
      and rCurrentRecord <> ? then
    do:
      hQuery:reposition-to-rowid(rCurrentRecord) no-error.
      brTables:select-focused-row().
      apply 'value-changed' to brTables. 
    end.
  end. /* do with frame */

END PROCEDURE. /* reopenTableBrowse */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resizeFilters C-Win 
PROCEDURE resizeFilters :
/*------------------------------------------------------------------------
  Name         : resizeFilters
  Description  : Redraw the filters. This is needed when the window 
                 resizes, one of the columns resizes or the user scrolls
                 in the browse.

  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/

  define input parameter piPageNr as integer no-undo.

  define variable cFilterFields as character no-undo. 
  define variable cButtons      as character no-undo. 
  define variable hBrowse       as handle    no-undo.
  define variable iField        as integer   no-undo.
  define variable hField        as handle    no-undo.

  if piPageNr = ? then piPageNr = giCurrentPage.
  setWindowFreeze(yes).

  do with frame {&frame-name}:

    /* Make one string of all handles */
    case piPageNr:

      when 0 then do: /* Tables */
        assign 
          cFilterFields = substitute('&1,&2,&3,&4'
                                    , fiTableFilter:handle
                                    , cbDatabaseFilter:handle 
                                    , fiNumQueriesFilter:handle  
                                    , fiLastUsedFilter:handle
                                    )
          cButtons      = substitute('&1,&2'
                                    , btnClearTableFilter:handle
                                    , btnTableFilter:handle
                                    )
          hBrowse       = brTables:handle
          .

      end. /* 0 */

      when 1 then do: /* Fields */
        assign 
          cFilterFields = substitute('&1,&2,&3,&4,&5,&6'
                                    , tgSelAll:handle
                                    , fiOrderFilter:handle 
                                    , fiNameFilter:handle  
                                    , fiTypeFilter:handle  
                                    , fiFormatFilter:handle
                                    , fiLabelFilter:handle 
                                    )
          cButtons      = substitute('&1,&2'
                                    , btnClearFieldFilter:handle
                                    , btnFieldFilter:handle
                                    )
          hBrowse       = brFields:handle
          .
      end. /* 1 */

      when 2 then do: /* Indexes */
        assign 
          cFilterFields = substitute('&1,&2,&3'
                                    , fiIndexNameFilter:handle
                                    , fiFlagsFilter:handle 
                                    , fiFieldsFilter:handle  
                                    )
          cButtons      = substitute('&1,&2'
                                    , btnClearIndexFilter:handle
                                    , btnIndexFilter:handle
                                    )
          hBrowse       = brIndexes:handle
          .
      end. /* 2 */
    end case. /* giCurrentPage */


    /* Save current widths to registry */
    fieldLoop:
    do iField = 1 to num-entries(cFilterFields):
      hField = hBrowse:get-browse-column(iField).

      setRegistry('DataDigger'
                 , substitute('ColumnWidth:&1', hField:name)
                 , substitute('&1', hField:width-pixels)
                 ).
    end.


    /* Resize them */
    run resizeFilterFields
      ( input cFilterFields
      , input cButtons
      , input hBrowse
      ).
  end.

  run showScrollBars(frame {&frame-name}:handle, no, no).
  setWindowFreeze(no).

END PROCEDURE. /* resizeFilters */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectClickedRow C-Win 
PROCEDURE selectClickedRow :
/*------------------------------------------------------------------------
  Name         : selectClickedRow
  Description  : Select the row the user last clicked on

  ----------------------------------------------------------------------
  20-05-2010 pti Created
  ----------------------------------------------------------------------*/
  define input  parameter phBrowse        as handle    no-undo.
  define output parameter plOk            as logical   no-undo.
  define output parameter pcColumnName    as character no-undo.

  define variable dRow             as decimal   no-undo.
  define variable iMouseX          as integer   no-undo.
  define variable iMouseY          as integer   no-undo.
  define variable iColumn          as integer   no-undo.
  define variable iExtentNr        as integer   no-undo.
  define variable hColumn          as handle    no-undo.
  define variable iRow             as integer   no-undo.
  define variable cColumnValue     as character no-undo.
  define variable hBuffer          as handle    no-undo.

  define buffer ttField for ttField. 

  /* If there is nothing in the browse, just return. */
  if phBrowse:query:num-results = 0 then do:
    return. 
  end.

  /* Get mouse position (but not if we used SHIFT-F10 for the context menu */
  if last-event:label = 'SHIFT-F10' then
  do:
    iRow = phBrowse:focused-row.
  end.

  else /* used mouse right click */
  do:
    run getMouseXY ( input phBrowse:frame 
                   , output iMouseX
                   , output iMouseY
                   ).

    /* Find out what row number we clicked on */
    dRow = (iMouseY - phBrowse:y - 18) / (phBrowse:row-height-pixels + 4).
    iRow = (if dRow = integer(dRow) then integer(dRow) else truncate(dRow,0) + 1). /* ceiling of dRow */

    /* Is it a valid row nr? (could be invalid if we clicked below last record) */
    if iRow > phBrowse:num-iterations then return.
    if iRow < 1 then return.

    /* Get the record in the buffer */
    phBrowse:select-row(iRow).
    phBrowse:fetch-selected-row(phBrowse:num-selected-rows).
    plOk = true.

    /* Find out which column we clicked on */
    findColumn:                                                                         
    do iColumn = 1 to phBrowse:num-columns:                                              
      find ttField where ttField.hColumn = phBrowse:get-browse-column(iColumn) no-error. 
      if not available ttField then next.                                                

      if    (iMouseX - phBrowse:x) > ttField.hColumn:x
        and (iMouseX - phBrowse:x) < (ttField.hColumn:x + ttField.hColumn:width-pixels) then 
      do: 
        /* pcColumnName = entry(1,hColumn:name,'['). /* if it is an extent, just grab fieldname */ */
        pcColumnName = ttField.cFullName.

        /* At this moment, we have the record the user clicked on in the buffer. 
         * Use it to retrieve the name and value of the columns the user clicked on
         */
        hBuffer = phBrowse:query:get-buffer-handle(1).
        case pcColumnName:
          when 'RECID' then cColumnValue = string( hBuffer:recid ).
          when 'ROWID' then cColumnValue = string( hBuffer:rowid ).
          otherwise cColumnValue = hBuffer:buffer-field(ttField.cFieldName):buffer-value(ttField.iExtent).
        end. 

        leave findColumn.
      end. 
    end. /* findColumn */
  end. /* used the mouse */

  /* Save the column value to be able to add it to filters */
  phBrowse:private-data = pcColumnName + chr(1) + cColumnValue + chr(1) + string(iExtentNr).

end procedure. /* selectClickedRow */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setConnectionMenu C-Win 
PROCEDURE setConnectionMenu :
/*------------------------------------------------------------------------
  Name         : setConnectionMenu
  Description  : Rebuild the connection submenu of the 'add' button

  ----------------------------------------------------------------------
  18-09-2009 pti Created
  ----------------------------------------------------------------------*/

  /* Attach connections to btnConnect */
  define variable hMenuItem       as handle      no-undo.
  define variable cProgDir        as character   no-undo.
  define variable cConnectionList as character   no-undo.
  define variable cDatabase       as character   no-undo.
  define variable iConn           as integer     no-undo.
  define variable hItemToDelete   as handle      no-undo. 

  hMenuItem = brTables:popup-menu:first-child in frame {&frame-name}.
  cProgDir = getProgramDir().

  /* Remove all current items except first 3 */
  do while valid-handle(hMenuItem):
    if hMenuItem:dynamic then hItemToDelete = hMenuItem.
    hMenuItem = hMenuItem:next-sibling.
    if valid-handle(hItemToDelete) then 
      delete object hItemToDelete.
  end.

  /* Get list of connections */
  run value(cProgDir + 'wConnections.w') 
    ( input 'getConnections'
    , input ''
    , output cConnectionList
    ).

  /* And add them to the menu */
  do iConn = 1 to num-entries(cConnectionList):
    cDatabase = entry(iConn,cConnectionList).

    /* Skip if already connected */
    if not connected(cDatabase) then 
      create menu-item hMenuItem
        assign
          label  = cDatabase
          name   = cDatabase
          parent = brTables:popup-menu
        triggers:
          on 'CHOOSE':U persistent run connectDatabase in this-procedure (cDatabase).
        end triggers.

  end. /* do iConn */

end procedure. /* setConnectionMenu */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setCounter C-Win 
PROCEDURE setCounter :
/*------------------------------------------------------------------------
  Name         : setCounter
  Description  : Set the counter to a certain number with small animation

  ----------------------------------------------------------------------
  25-01-2011 pti Created
  ----------------------------------------------------------------------*/

  DEFINE input PARAMETER piCounter AS integer NO-UNDO.

  DEFINE VARIABLE cNewDigit AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cOldDigit AS CHARACTER   NO-UNDO.

  DEFINE VARIABLE iMove      AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iPos       AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iStartingY AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iStartTime AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iNumDigits AS INTEGER     no-undo initial {&numDigits}.

/*
&global-define numDigits   6
&global-define digitWidth  8
&global-define digitHeight 15
&global-define marginHor   3
&global-define marginVer   3
*/

  if piCounter < 0 then piCounter = 0.
  piCounter = piCounter modulo 1000000.

  do with frame {&frame-name}:
    iStartingY = ghNewDigit[iNumDigits]:y.
    cNewDigit = string(piCounter,fill('9',iNumDigits)).
    cOldDigit = string(maximum(0,piCounter - 1), fill('9',iNumDigits)).

    /* Prepare the screen */
    do iPos = 1 to iNumDigits:
      /* Set digits on proper line with proper height and label */
      ghOldDigit[iPos]:y = iStartingY.
      ghNewDigit[iPos]:y = iStartingY.
      ghOldDigit[iPos]:height-pixels = {&digitHeight}.
      ghNewDigit[iPos]:height-pixels = {&digitHeight}.
      ghOldDigit[iPos]:screen-value = substring(cOldDigit,iPos,1).
      ghNewDigit[iPos]:screen-value = substring(cNewDigit,iPos,1).

      /* Hide leading zeros of old value */
      ghOldDigit[iPos]:visible = integer(substring(cOldDigit,1,iPos)) > 0.
      ghNewDigit[iPos]:visible = no.
    end.

    do iMove = 1 to {&digitHeight} by 1:
      do iPos = 1 to iNumDigits:
        if substring(cOldDigit,iPos,1) = substring(cNewDigit,iPos,1) then next.

        /* Move the old number down, making it smaller */
        ghOldDigit[iPos]:y             = iStartingY + iMove.
        ghOldDigit[iPos]:height-pixels = maximum(1,{&digitHeight} - iMove).
        ghOldDigit[iPos]:visible       = (iMove < {&digitHeight}).

        /* And insert the new number, making it larger */
        ghNewDigit[iPos]:height-pixels = iMove.
        ghNewDigit[iPos]:visible       = yes.
      end.

      /* Enforce a small pause */
      iStartTime = etime.
      repeat while etime < iStartTime + 15. end.
    end.

    /* Reset all images to their proper place */
    do iPos = 1 to iNumDigits:

      /* Show Oldious value at the location of the digits */
      ghOldDigit[iPos]:y = iStartingY.
      ghNewDigit[iPos]:y = iStartingY.

      ghOldDigit[iPos]:height-pixels = {&digitHeight}.
      ghNewDigit[iPos]:height-pixels = {&digitHeight}.

      /* Hide leading zeros of old value */
      ghOldDigit[iPos]:visible      = no.
      ghOldDigit[iPos]:screen-value = ghNewDigit[iPos]:label.
      ghNewDigit[iPos]:visible      = integer(substring(cNewDigit,1,iPos)) > 0.
      ghNewDigit[iPos]:tooltip      = substitute('served &1 queries~n... and counting', piCounter).
    end.
  end.

END PROCEDURE. /* setCounter */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setDataFilter C-Win 
PROCEDURE setDataFilter :
/*------------------------------------------------------------------------
  Name         : setDataFilter
  Description  : Optionally clear the filters and set a filter value

  ----------------------------------------------------------------------
  18-09-2009 pti Created
  ----------------------------------------------------------------------*/

  define input parameter plClearOtherFilters as logical     no-undo.

  define variable cColumnName  as character   no-undo.
  define variable cColumnValue as character   no-undo.

  define buffer ttField for ttField.

  /* Freeze updates */
  setWindowFreeze(yes).

  if num-entries(ghDataBrowse:private-data,chr(1)) = 3 then 
  do:
    cColumnName  = entry(1, ghDataBrowse:private-data,chr(1)).
    cColumnValue = entry(2, ghDataBrowse:private-data,chr(1)).

    for each ttField 
      where ttField.lDataField = true
        and valid-handle(ttField.hFilter):

      /* If this is the field we're looking for, set the 
       * value. Otherwise see if we need to blank other fields.
       */
      if ttField.cFullName = cColumnName then 
        ttField.hFilter:screen-value = cColumnValue.
      else
      if plClearOtherFilters then
        ttField.hFilter:screen-value = ttField.hFilter:private-data.

      setFilterFieldColor(ttField.hFilter).
      run filterFieldValueChanged(ttField.hFilter).
    end.

    run reopenDataBrowse('',?).
  end. 

  setWindowFreeze(no).

end procedure. /* setDataFilter */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setDbContext C-Win 
PROCEDURE setDbContext :
/*------------------------------------------------------------------------
  Name         : setDbContext 
  Description  : Fill the list of tables and get last used table name 
                 and user query from registry.
  ---------------------------------------------------------------------- 
  18-03-2009 pti Created
  ----------------------------------------------------------------------*/

  define input parameter pcDatabase as character   no-undo.

  define variable cTableList as character   no-undo.
  define variable cQuery     as character   no-undo.
  define variable cTable     as character   no-undo.

  do with frame {&frame-name}:

    /* Save last used db in registry */
    if pcDatabase = '' then
      setRegistry('DataDigger', 'Database',  '<empty>').
    else
      setRegistry('DataDigger', 'Database',  pcDatabase).

    /* Collect list of tables in this db */
    cTableList = getTableList( pcDatabase
                             , getMatchesValue(fiTableFilter:handle)
                             , menu-item m_Show_hidden_tables:checked in menu POPUP-MENU-brTables /* tHiddenTables:checked */
                             , ?
                             , ?
                             ).

    run reopenTableBrowse(?,?,?).

    btnViewData:sensitive = false. /* Until we know there is a table, VIEW is off */
    btnViewData-2:sensitive in frame frWhere = false.
    btnWhere:sensitive    = false. 
    if valid-handle (ghDataBrowse) then ghDataBrowse:hidden = true.

    /* Restore last used table for this db. If you switch back to a database, 
     * you want to see the last used table for that database. 
     */
    if pcDatabase <> ? then
      cTable = getRegistry ('DB:' + cbDatabaseFilter:screen-value, 'table').
    else 
      assign cTable = ''.

    /* Unknown table or 'All databases' selected, then just pick the first */
    if cTable = ? or lookup(cTable,cTableList) = 0 then 
      assign cTable = entry(1,cTableList).

    /* Make sure globals are set */
    apply 'value-changed' to brTables.

    run setTableContext(input cTable).
  end.  

end procedure. /* setDbContext */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPage C-Win 
PROCEDURE setPage :
/*------------------------------------------------------------------------
  Name         : setPage
  Description  : Activate either the fields browse or the indexes browse. 

  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piPage AS INTEGER     NO-UNDO.

  /* If we are already on this page, then we're ready */
  if giCurrentPage = piPage then return.

  /* remember page */
  giCurrentPage = piPage.
  setRegistry("DataDigger", "ActivePage", string(giCurrentPage)).

  setWindowFreeze(yes).

  do with frame {&frame-name}:
    case piPage:
      when 1 then do:
        btnTabFields :load-image( getImagePath('tab_fields_active.gif'    )).
        btnTabIndexes:load-image( getImagePath('tab_indexes_inactive.gif' )).
        view {&list-2}.
        hide {&list-3}.

        run setRedLines.

        if not valid-handle(ghDataBrowse) then
          hide btnClearDataFilter btnDataFilter.

        btnRemoveFilter:sensitive = (gcFieldFilterList <> "").
      end.

      when 2 then do:
        btnTabFields :load-image( getImagePath('tab_fields_inactive.gif' )).
        btnTabIndexes:load-image( getImagePath('tab_indexes_active.gif'  )).
        hide {&list-2}.                             
        view {&list-3}.   

        run setRedLines.

        btnRemoveFilter2:sensitive = (gcFieldFilterList <> "").
      end.                                          
    end case. /* piPage */

    run resizeFilters(input piPage).
  end.

  run showScrollBars(frame {&frame-name}:handle, no, no).
  setWindowFreeze(no).
END PROCEDURE. /* setPage */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setRedLines C-Win 
PROCEDURE setRedLines :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  do with frame {&frame-name}:

    /* Tables */
    if getMatchesValue(fiTableFilter:handle) <> '*'
      /* or cbDatabaseFilter:screen-value <> ? */
      or integer(fiNumQueriesFilter:screen-value) <> 0
      or gcFieldFilterList <> '' then
      rcTableFilter:visible = true.
    else 
      rcTableFilter:visible = false.

    case giCurrentPage:
      when 1 then do:
        /* Fields */
        if (fiOrderFilter:screen-value <> fiOrderFilter:private-data) 
          or getMatchesValue(fiNameFilter:handle) <> '*'
          or getMatchesValue(fiTypeFilter:handle) <> '*' 
          or getMatchesValue(fiFormatFilter:handle) <> '*'  
          or getMatchesValue(fiLabelFilter :handle) <> '*' then
          rcFieldFilter:visible = true.
        else 
          rcFieldFilter:visible = false.
      end.

      when 2 then do:
       /* Index */
       if   getMatchesValue(fiIndexNameFilter:handle) <> '*'
         or getMatchesValue(fiFlagsFilter    :handle) <> '*' 
         or getMatchesValue(fiFieldsFilter   :handle) <> '*' then 
         rcIndexFilter:visible = true.
        else 
          rcIndexFilter:visible = false.
      end.
    end.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTableContext C-Win 
PROCEDURE setTableContext :
/*------------------------------------------------------------------------
  Name         : setTableContext
  Description  : Perform actions when a change of table has occurred. 
                 Reread the fields, indexes, selected fields. 
                 Change title of window
  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/

  define input parameter pcTable as character no-undo. 

  define variable cFieldList   as character   no-undo.
  define variable cQuery       as character   no-undo.
  define variable lTableChange as logical     initial FALSE no-undo.

  do with frame {&frame-name}:

    /* If the tablename is different from what is in the screen,
     * adjust the latter one.
     */
    if pcTable <> getCurrentTable() then
      setCurrentTable( pcTable ).

    /* Delete existing browse */
    delete object ghDataBrowse no-error.

    /* Delete filters */
    run deleteDataFilters.

    /* Disable edit panel */
    setUpdatePanel('no-record').

    /* Refill the tt with fields of this table */
    run collectFieldInfo(input pcTable).

    /* Refill the index tt */
    run collectIndexInfo(input pcTable).

    /* Get all saved queries of this table */
    run collectQueryInfo( input getCurrentDatabase(), input pcTable ).
    assign giQueryPointer = 1.

    /* Reopen the queries */
    run reopenFieldBrowse(?,?).
    run reopenIndexBrowse(?,?).

    /* Set toggle to de/select all fields */
    tgSelAll:checked = true.

    /* Unless no field is selected */
    if getSelectedFields() = '' then tgSelAll:checked = false.

    /* Get a list of all fields (extents expanded) */
    for each ttField by ttField.cFieldName by ttField.iExtent:
      .if ttField.lMetaField = no  and ttField.iExtent > 0 then next.
      .if ttField.lMetaField = yes and ttField.iExtent = 0 then next.
      cFieldList = cFieldList + ',' + ttField.cFullname.
    end.

    do with frame frWhere:
      /* Set list of fields in field combo */
      cbFields:list-items     = cFieldList.
      cbAndOr:screen-value    = entry(1,cbAndOr:list-items).
      cbFields:screen-value   = entry(1,cbFields:list-items).
      cbOperator:screen-value = entry(1,cbOperator:list-items).
    end.

    /* Restore last used query */
    /* dit maar even uit. is eigenlijk best irritant namelijk */
    assign giQueryPointer = 0.

    /* Expand chr(1) to RETURN and - if query editor is collapsed - RETURN to chr(160) */
    cQuery = replace(cQuery,chr(1),'~n').
    if gcQueryEditorState = 'hidden' then
      cQuery = replace(cQuery, '~n', chr(160)).
    ficWhere:screen-value = cQuery.

    fiWarning:visible = no.
    ficWhere:bgcolor = ?. /* default */
    ficWhere:fgcolor = ?. /* default */
    ficWhere:tooltip = ''.

    /* Save last used table and position in browse in registry */
    setRegistry ("DB:" + getCurrentDatabase(), "table", pcTable ).

    run setWindowTitle.

  end.

end procedure. /* setTableContext */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setViewType C-Win 
PROCEDURE setViewType :
/*------------------------------------------------------------------------
  Name         : setViewType
  Description  : Set the type of view to view records (TXT HTML XLS)
  ----------------------------------------------------------------------
  10-09-2010 pti Created
  ----------------------------------------------------------------------*/

  define input  parameter pcViewType as character   no-undo.

  do with frame frMain:

    btnView:label = substitute('View:&1',pcViewType).

    setRegistry('DataDigger', 'ViewType', pcViewType).
  end.

END PROCEDURE. /* setViewType */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setWindowTitle C-Win 
PROCEDURE setWindowTitle :
/*------------------------------------------------------------------------
  Name         : setWindowTitle
  Description  : Set the title of the DataDigger window
  ----------------------------------------------------------------------
  17-02-2011 pti Created
  ----------------------------------------------------------------------*/

  define variable cTitle       as character no-undo.
  define variable hParent      as integer   no-undo.
  define variable hOwner       as integer   no-undo.

  /*
  ** Display the current database and table name in the windowtitle
  */
  cTitle = substitute( "&1 &2 &3 - &4.&5 &6" 
                     , "DataDigger"
                     , "{&buildnr}"
                     , (if session:parameter <> '' then '- ' + session:parameter else '')
                     , getCurrentDatabase()
                     , getCurrentTable()
                     , (if gcFieldFilterList <> '' then '(' + gcFieldFilterList + ')'  else '')
                     ).
  C-Win:title = cTitle.

  run GetParent (c-win:hwnd, output hParent).
  run GetWindow (hParent, 4, output hOwner).
  run SetWindowTextA ( hOwner, cTitle ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showField C-Win 
PROCEDURE showField :
/*------------------------------------------------------------------------
  Name         : showField
  Description  : Toggle the selected status of a field. 

  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/

  define input parameter pcFieldName as character no-undo. 
  define input parameter plSelected  as logical   no-undo. 

  define buffer ttField for ttField. 

  setWindowFreeze(yes).

  do with frame {&frame-name}:
    for each ttField 
        where ttField.cFullName matches pcFieldName:

      ttField.lShow = (if plSelected = ? then not ttField.lShow else plSelected).

      /* Hide data columns */
      if valid-handle(ttField.hColumn) then 
      do:
        ttField.hColumn:visible = ttField.lShow.
        run dataScrollNotify(input ghDataBrowse).
      end.

      /* This solves a strange error:
       * Uncheck a field in the field browse, leave focus on the checkbox
       * Right click on data browse, choose 'Unhide all'
       * Now all fields unhide, except the one with focus.
       */
      if ttField.cFieldName = brFields:get-browse-column(3):screen-value then 
        brFields:get-browse-column(1):checked = ttField.lShow.

    end. /* f/e ttField */

    /* If we (de)selected using ENTER/SPACE, go to the next row */
    if last-event:event-type = "KEYPRESS" 
      and (last-event:code = 32 or last-event:code = 13) then 
      brFields:select-next-row(). 

    saveSelectedFields().
    brFields:refresh().
    run dataScrollNotify(ghDataBrowse).
  end.

  setWindowFreeze(no).

END PROCEDURE. /* showField */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showValue C-Win 
PROCEDURE showValue :
/*------------------------------------------------------------------------
  Name         : showValue
  Description  : Show the value of the current cell
  ---------------------------------------------------------------------- 
  18-01-2011 pti Created
  ----------------------------------------------------------------------*/

  define variable cColumnName  as character   no-undo.
  define variable cColumnValue as character   no-undo.

  if num-entries(ghDataBrowse:private-data,chr(1)) <> 3 then return. 

  cColumnName  = entry(1, ghDataBrowse:private-data,chr(1)).
  cColumnValue = entry(2, ghDataBrowse:private-data,chr(1)).

  if cColumnValue <> '' and cColumnValue <> ? then 
    message trim(cColumnValue)
      view-as alert-box info buttons ok.

end procedure. /* showValue */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE startDiggerLib C-Win 
PROCEDURE startDiggerLib :
/*------------------------------------------------------------------------
  Name         : startDiggerLib
  Description  : Start DiggerLib if it has not already been started

  ----------------------------------------------------------------------
  21-10-2009 pti Created
  ----------------------------------------------------------------------*/


  define variable cProgDir     as character   no-undo.
  define variable cLibVersion  as character   no-undo.

  /* Call out to see if the lib has been started for this build nr */
  publish 'DiggerLib' (output hDiggerLib). 

  /* Not yet started? */
  if not valid-handle(hDiggerLib) then
  do:
    file-info:file-name = this-procedure:file-name.
    cProgDir = substring(file-info:full-pathname,1,r-index(file-info:full-pathname,'\')).
    /* debug */
    .if '{&uib_is_running}' <> '' then cProgDir = 'd:\data\progress\datadigger\'.
    /* debug */

    run value(getProgramDir() + 'DataDiggerLib.p') persistent set hDiggerLib.
    session:add-super-procedure(hDiggerLib,search-target).

    return.
  end.

  /* If the lib was already started, it might be a lib of a previous version if you
   * install DataDigger over a running version. To avoid errors, we get the version 
   * of the running instance and see if it matches the version of the window. 
   * If not, close all other windows (these are old and if we restart the lib then
   * in turn THEY will be out of sync with the lib) and restart the lib.
   */
  run getLibVersion in hDiggerLib(output cLibVersion) no-error.

  if   cLibVersion <> '{&BuildNr}' then
  do:
    /* Publish a close to all open digger windows. The one that issues
     * this publish will not be closed because we are not subscribed yet.
     */
    publish 'DataDiggerClose'.
    delete procedure hDiggerLib.
    run startDiggerLib.
  end.

end procedure. /* startDiggerLib */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE welcome C-Win 
PROCEDURE welcome :
/*------------------------------------------------------------------------
  Name         : welcome
  Description  : Show a welcome message to the user. 

  ----------------------------------------------------------------------
  07-09-2009 pti Created
  ----------------------------------------------------------------------*/

  define variable cSetting as character no-undo. 

  /* Check on DataDigger version */
  cSetting = getRegistry('DataDigger', 'Version').
  if cSetting <> '{&BuildNr}' then 
  do:
    /* Save last used version nr */
    setRegistry('DataDigger', 'Version', '{&BuildNr}').

    run showHelp('NewVersion', '{&BuildNr}').

    if getRegistry('DataDigger:help', 'NewVersion:answer') = '1' then
      apply 'choose' to btnChangeLog in frame frSettings.
  end.

  /* Check on the use of -rereadnolock */
  if lookup('-rereadnolock', session:startup-parameters) = 0 then 
    run showHelp('RereadNoLock', '').

  /* Check on font (mis)usage */
  if isDefaultFontsChanged() then
    run showHelp('FontsChanged','').

end procedure. /* welcome */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION enableTimers C-Win 
FUNCTION enableTimers RETURNS LOGICAL
  ( input plTimersEnabled as logical ) :

/*------------------------------------------------------------------------
  Name         : enableTimers
  Description  : Enable or disable all timers

  ----------------------------------------------------------------------
  24-01-2011 pti Created
  ----------------------------------------------------------------------*/

  chFilterTimer:FilterTimer:enabled = plTimersEnabled.
  chCtrlFrame:PSTimer:enabled       = plTimersEnabled.

  return true.

end function. /* enableTimers */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getActiveQueryEditor C-Win 
FUNCTION getActiveQueryEditor RETURNS HANDLE
  ( /* parameter-definitions */ ) :

/*------------------------------------------------------------------------
  Name         : getActiveQueryEditor
  Description  : Return the handle of the active query editor

  ----------------------------------------------------------------------
  07-09-2009 pti Created
  ----------------------------------------------------------------------*/

  /* If the query editor is expanded, do actions to that field */
  if gcQueryEditorState = 'hidden' then 
    return ficWhere:handle in frame frMain.
  else 
    return ficWhere2:handle in frame frWhere.

end function. /* getActiveQueryEditor */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCurrentDatabase C-Win 
FUNCTION getCurrentDatabase RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  do with frame {&frame-name}:
    /* gcCurrentDatabase */
    if brTables:query:get-buffer-handle(1):available then
      return brTables:query:get-buffer-handle(1)::cdatabase.
    else 
      return ''.

  end.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCurrentTable C-Win 
FUNCTION getCurrentTable RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  define variable hBuffer as handle no-undo.

  do with frame {&frame-name}:
    hBuffer = brTables:query:get-buffer-handle(1).
    if hBuffer:available then
      return hBuffer::cTableName.
    else 
      return "".
  end.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDataQuery C-Win 
FUNCTION getDataQuery RETURNS CHARACTER
  ( /* parameter-definitions */ ) :

/*------------------------------------------------------------------------
  Name         : getDataQuery
  Description  : Return the query that belongs to the currently shown data
  ---------------------------------------------------------------------- 
  04-02-2011 pti Created
  ----------------------------------------------------------------------*/

  define variable cDatabase    as character   no-undo.
  define variable cTable       as character   no-undo.
  define variable cQuery       as character   no-undo.
  define variable cWhere       as character   no-undo.
  define variable cSort        as character   no-undo.
  define variable cAndWhere    as character   no-undo.
  define variable cFilter      as character   no-undo.

  cDatabase = gcCurrentDatabase.
  cTable    = gcCurrentTable.
  cFilter   = getFilterQuery().

  /* Get query from editor */
  cWhere = trim(ficWhere:screen-value in frame {&frame-name}).
  cWhere = replace(cWhere, chr(160), '~n').

  /* If a query starts with 'AND' or 'OR' or 'WHERE', strip it */
  /* Save the user-query and set the pointer to 1 */
  run saveQuery( cDatabase, cTable, cWhere).

  if lookup(entry(1,cWhere,' '),'AND,OR,WHERE') > 0 then
    entry(1,cWhere,' ') = ''.

  /* Extract the sort-by part */
  if index(cWhere, 'BY ') > 0 then
    assign cSort  = substring(cWhere,index(cWhere, 'BY '))
           cWhere = replace(cWhere,cSort,'').

  /* Now, lets build it up. Start with the basics */
  cQuery = substitute("for each &1.&2 no-lock", cDatabase, cTable).

  /* Add query filter */
  if cFilter <> '' then
    cQuery = substitute("&1 WHERE (&2)", cQuery, cFilter).

  /* Add the where  */
  if cFilter =  '' and cWhere <> '' and not cWhere begins 'BY ' then cAndWhere = 'WHERE'.
  if cFilter <> '' and cWhere <> '' and not cWhere begins 'BY ' then cAndWhere = 'AND'.
  if cWhere <> '' then
    cQuery = substitute("&1 &2 (&3)", cQuery, cAndWhere, cWhere).

  /* Add sort */
  if cSort <> '' then
    cQuery = substitute("&1 &2", cQuery, cSort).

  /* For speed of repositioning... */
  cQuery = substitute("&1 INDEXED-REPOSITION", cQuery).

  return cQuery. 

end function. /* getDataQuery */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFieldList C-Win 
FUNCTION getFieldList returns character
  ( pcSortBy as character ) :

/*------------------------------------------------------------------------
  Name         : getFieldList
  Description  : Return a comma separated list of all fields.
  ---------------------------------------------------------------------- 
  09-07-2009 pti Created
  ----------------------------------------------------------------------*/

  define variable cFieldList as character  no-undo.
  define buffer ttField for ttField.

  define query qField for ttField.

  query qField:query-prepare(substitute('for each ttField by &1', pcSortBy)).
  query qField:query-open.
  query qField:get-first.

  /* All fields */
  repeat while not query qField:query-off-end:
    cFieldList = cFieldList + ',' + ttField.cFieldName.
    query qField:get-next.
  end.
  query qField:query-close.

  cFieldList = left-trim(cFieldList, ",").

  return cFieldList.   /* Function return value. */

end function. /* getFieldList */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFilterQuery C-Win 
FUNCTION getFilterQuery RETURNS CHARACTER
  ( /* parameter-definitions */ ) :

/*------------------------------------------------------------------------
  Name         : getFilterQuery
  Description  : Return a query built from fields in the filter fields
  ---------------------------------------------------------------------- 
  27-10-2009 pti Created
  ----------------------------------------------------------------------*/

  define variable cFilterQuery as character   no-undo.
  define variable cOperator    as character   no-undo.

  define buffer ttField for ttField. 
  define buffer bField for ttField. 

  /* Collect all filters */
  cFilterQuery = ''.
  for each ttField where ttField.lShow:

    if   ttField.cFilterValue = ttField.cFullName
      or ttField.cFilterValue = '' 
      or ttField.cFilterValue = ?  then next.

    /* If it is an extent field, find the template and see if that one is selected */
    if ttField.iExtent > 0 and 
      not can-find(first bField where bField.cFieldName = ttField.cFieldName
                                  and bField.lMetaField   = false
                                  and bField.lShow      = true) then next. 

    if ttField.cDataType = 'CHARACTER'
      and index(ttField.cFilterValue,'*') > 0 then
      cOperator = 'matches'.
    else 
      cOperator = '='.

    cFilterQuery = substitute('&1 &2 &3 &4 &5'
                        , cFilterQuery
                        , if cFilterQuery = '' then '' else 'AND'
                        , ttField.cFullName
                        , cOperator
                        , quoter(ttField.cFilterValue)
                        ).
  end.

  publish 'message' (1,substitute('Query From Filter: &1', cFilterQuery)).

  return cFilterQuery.   /* Function return value. */

end function. /* getFilterQuery */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&IF DEFINED(EXCLUDE-getProgramDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getProgramDir Procedure 
FUNCTION getProgramDir RETURNS CHARACTER
  ( /* parameter-definitions */ ) :

/*------------------------------------------------------------------------
  Name         : getProgramDir
  Description  : Return the directory in which DataDigger is installed,
                 including a backslash
  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/

  define variable cProgDir   as character   no-undo.

  file-info:file-name = this-procedure:file-name.

  /* If you use a compiled version without the .p around
   * the full-pathname is unknown. In that case use the 
   * name of the program with a .r suffix 
   */
  if this-procedure:file-name matches '*.p' 
    and file-info:full-pathname = ? then
    file-info:file-name = replace(this-procedure:file-name,'.p','.r').

  cProgDir = substring(file-info:full-pathname,1,r-index(file-info:full-pathname,'\')).
  cProgDir = "util\DataDigger\".
  if '{&uib_is_running}' <> '' then cProgDir = '.\util\DataDigger\'.

  RETURN cProgDir. /* Function return value. */

END FUNCTION. /* getProgramDir */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTempDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTempDir Procedure 
FUNCTION getTempDir RETURNS CHARACTER
  ( /* parameter-definitions */ ) :

/*------------------------------------------------------------------------
  Name         : getTempDir
  Description  : Return the directory in which DataDigger is installed,
                 including a backslash
  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/

  define variable cProgDir   as character   no-undo.

  file-info:file-name = this-procedure:file-name.

  /* If you use a compiled version without the .p around
   * the full-pathname is unknown. In that case use the 
   * name of the program with a .r suffix 
   */

  cProgDir = "c:\tmp\".


  RETURN cProgDir. /* Function return value. */

END FUNCTION. /* getProgramDir */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getQueryFromFields C-Win 
FUNCTION getQueryFromFields returns character
  ( input pcFieldList as character ):

/*------------------------------------------------------------------------
  Name         : getQueryFromFields
  Description  : Return a query built from fields in a list
  ---------------------------------------------------------------------- 
  20-10-2009 pti Created
  ----------------------------------------------------------------------*/

  define variable cField         as character   no-undo.
  define variable iField         as integer   no-undo. 
  define variable cNameFormat    as character no-undo. 
  define variable cFieldList     as character no-undo. 
  define variable cQuery         as character no-undo. 

  /* Determine format for names */
  cNameFormat = fill('x', getMaxLength(pcFieldList) ).

  /* Build query */
  cQuery = ''.
  do iField = 1 to num-entries(pcFieldList):
    cField = entry(iField,pcFieldList).
    find ttField where ttField.cFieldName = cField.
    cQuery = substitute('&1&2 &3 = &4'
                       , cQuery 
                       , (if iField = 1 then 'where' else '~n  and') 
                       , string(cField,cNameFormat) 
                       , quoter(getLinkInfo(cField))
                       ).
  end.

  publish 'message' (1,substitute('Query From Fields: &1', cQuery)).

  return cQuery.

end function. /* getQueryFromFields */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSelectedFields C-Win 
FUNCTION getSelectedFields returns character
  ( /* parameter-definitions */ ) :

/*------------------------------------------------------------------------
  Name         : getSelectedFields
  Description  : Return all selected fields. 

  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/

  define variable cSelectedFields as character  no-undo.
  define buffer ttField for ttField.

  /* All selected fields */
  for each ttField where ttField.lShow = true 
    by ttField.iOrder:

    cSelectedFields = cSelectedFields + ',' + ttField.cFieldName.
  end.

  cSelectedFields = left-trim(cSelectedFields, ",").

  return cSelectedFields. 

end function. /* getSelectedFields */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION saveSelectedFields C-Win 
FUNCTION saveSelectedFields RETURNS CHARACTER
  ( /* parameter-definitions */ ) :

/*------------------------------------------------------------------------
  Name         : saveSelectedFields
  Description  : Write the selected fields to the INI

  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/

  define variable cTable          as character no-undo. 
  define variable cSelectedFields as character no-undo.

  do with frame {&frame-name}:

    /* Get the selected fields to display in the browse */        
    cTable          = getCurrentTable().
    cSelectedFields = getSelectedFields().

    /* If no fields are selected, use a special marker */
    if cSelectedFields = '' then cSelectedFields = '<none>'.

    /* If all fields are selected, we don't save the setting */
    if num-entries(cSelectedFields) = num-entries(getFieldList('cFieldName')) then 
      cSelectedFields = ?.

    /* Save selected fields */
    setRegistry( substitute('DB:&1',getCurrentDatabase())
               , substitute('&1:fields',cTable)
               , cSelectedFields
               ).
  end.

  return "".   /* Function return value. */

END FUNCTION. /* saveSelectedFields */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setCurrentTable C-Win 
FUNCTION setCurrentTable returns logical
  ( pcTableName as character ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  define buffer bTable for ttTable.

  do with frame {&frame-name}:
    find bTable 
      where bTable.cDatabase  = getCurrentDatabase()
        and bTable.cTableName = pcTableName
            no-error.
    if not available bTable then return no.

    brTables:query:reposition-to-rowid( rowid(bTable) ) no-error.
    .brTables:select-focused-row().
  end.

end function. /* setCurrentTable */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setNumRecords C-Win 
FUNCTION setNumRecords returns logical
  ( input piNumRecords    as integer
  , input plCountComplete as logical 
  , input piQueryMSec     as integer) :

  define variable cNumResults as character   no-undo.

  do with frame {&frame-name}:
    fiNumresults:visible = true.

    if plCountComplete then 
      fiNumResults:fgcolor = 2. /* green */
    else 
      fiNumResults:fgcolor = 12. /* red */

    if plCountComplete then
      cNumResults = substitute(' &1 records in &2 msec', piNumRecords, piQueryMSec).
    else 
      cNumResults = substitute(' > &1 records', piNumRecords).

    fiNumResults:x = 100. /* park it to the left so we can expand it */
    fiNumResults:width-pixels = font-table:get-text-width-pixels(cNumResults) + 5.
    fiNumResults:x = rctData:x + rctData:width-pixels - fiNumResults:width-pixels - 40.
    fiNumResults:screen-value = cNumResults.
  end.

  return yes.   /* Function return value. */

end function. /* setNumRecords */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  ( piPointerChange as integer ) :

/*------------------------------------------------------------------------
  Name         : setQuery
  Description  : Fetches the previous or next query from the settings 
                 and fills it in in the query editor.

  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/

  define variable cQuery  as character no-undo. 
  define variable hEditor as handle    no-undo.

  hEditor = getActiveQueryEditor().

  /* See if the requested query exists */
  cQuery = getQuery(getCurrentDatabase(), getCurrentTable(), giQueryPointer + piPointerChange).

  if cQuery <> ? then 
  do:
    giQueryPointer = giQueryPointer + piPointerChange.
    hEditor:screen-value = formatQueryString(cQuery, gcQueryEditorState = 'visible').
  end.

  return cQuery <> ?.   /* Function return value. */

end function. /* setQuery */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQueryEditor C-Win 
FUNCTION setQueryEditor RETURNS LOGICAL
  ( pcQueryEditorState as character ) :

/*------------------------------------------------------------------------
  Name         : setQueryEditor
  Description  : Show or hide the query editor and associated fields. 

  ----------------------------------------------------------------------
  16-01-2009 pti Created
  ----------------------------------------------------------------------*/

  /* If we try to set it to its current value, nothing will happen so: */
  if pcQueryEditorState = gcQueryEditorState then return false.

  case pcQueryEditorState:
    when 'visible' then 
    do:

      if (ficWhere:x in frame frMain + frame frWhere:width-pixels) > c-win:width-pixels then
        frame frWhere:x = (c-win:width-pixels - frame frWhere:width-pixels) / 2.
      else
        frame frWhere:x = ficWhere:x.

      if (ficWhere:y in frame frMain + frame frWhere:height-pixels) > rctEdit:y in frame frMain then
        frame frWhere:y = rctEdit:y in frame frMain - frame frWhere:height-pixels - 20.
      else 
        frame frWhere:y = ficWhere:y.

      view frame frWhere.

      gcQueryEditorState = pcQueryEditorState.
      if ficWhere:screen-value in frame frMain <> '' then
        ficWhere2:screen-value in frame frWhere = formatQueryString(ficWhere:screen-value in frame frMain, yes). 
    end.

    when 'hidden'  then 
    do:
      hide frame frWhere.

      gcQueryEditorState = pcQueryEditorState.
      if ficWhere2:screen-value in frame frWhere <> '' then
        ficWhere:screen-value in frame frMain = formatQueryString(ficWhere2:screen-value in frame frWhere, no). 
    end.

    /* All other settings will be ignored */
    otherwise return false.

  end case.


  /* Save setting for query editor state */
  setRegistry("DataDigger", "QueryEditorState", gcQueryEditorState).

  return true.   /* Function return value. */

END FUNCTION. /* setQueryEditor */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setUpdatePanel C-Win 
FUNCTION setUpdatePanel RETURNS LOGICAL
  ( input pcMode as character ) :

/*------------------------------------------------------------------------------
  Purpose: setUpdatePanel
    Notes: enable / disable update panel buttons

    Mode       Sensitive buttons
    -----      --------------------
    display    add,delete,view,dump
    no-record  add
    update     save,cancel

------------------------------------------------------------------------------*/

  if pcMode <> ? then
    gcRecordMode = pcMode.

  do with frame frMain:

    assign 
      btnAdd:sensitive           = lookup( gcRecordMode, 'display,no-record') > 0  
      btnEdit:sensitive          = lookup( gcRecordMode, 'display') > 0 and ghDataBrowse:num-selected-rows > 0
      btnDelete:sensitive        = lookup( gcRecordMode, 'display') > 0 and ghDataBrowse:num-selected-rows > 0
      btnView:sensitive          = lookup( gcRecordMode, 'display') > 0 and ghDataBrowse:num-selected-rows > 0

      btnDump:sensitive          = lookup( gcRecordMode, 'display') > 0 
      btnDump-2:sensitive   in frame frSettings = lookup( gcRecordMode, 'display') > 0 
      btnDump-txt:sensitive in frame frSettings = lookup( gcRecordMode, 'display') > 0 
      .

    /* Load buttons not for version 16 */
    if getRegistry('DataDigger','LoadWindow') = 'yes' then
    assign
      btnLoad:sensitive          = lookup( gcRecordMode, 'display') > 0 
      btnLoad-2:sensitive   in frame frSettings = lookup( gcRecordMode, 'display') > 0 
      btnLoad-txt:sensitive in frame frSettings = lookup( gcRecordMode, 'display') > 0 
      .
    else 
    assign
      btnLoad:sensitive                         = false
      btnLoad-2:sensitive   in frame frSettings = false
      btnLoad-txt:sensitive in frame frSettings = false
      .

    /* Hide these when no record */
    assign
      fiIndexInfo:visible        = lookup( gcRecordMode, 'display,update') > 0     
      btnDataFilter:visible      = lookup( gcRecordMode, 'display,update') > 0     
      btnClearDataFilter:visible = lookup( gcRecordMode, 'display,update') > 0     
      fiNumresults:visible       = lookup( gcRecordMode, 'display,update') > 0     
      .

    /* For the time being, disable the ADD button */
    btnAdd:sensitive = false.
  end.

  /* Kill scrollbars */
  run showScrollBars(frame {&frame-name}:handle, no, no).

end function. /* setUpdatePanel */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setWindowFreeze C-Win 
FUNCTION setWindowFreeze RETURNS LOGICAL
  ( plWindowsLocked as logical ) :

/*------------------------------------------------------------------------------
  Purpose: setWindowFreeze
    Notes: enable / disable debug mode
------------------------------------------------------------------------------*/

  if glDebugMode then return no. 
  run LockWindow (input C-Win:handle, input plWindowsLocked).

  return true.

end function. /* setWindowFreeze */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

