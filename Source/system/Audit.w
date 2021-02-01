&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          audit            PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: Audit.w (also used as include in system/callAudit.p)

  Description: Audit History

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 10.18.2017

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

&Scoped-define MAX-ROWS MAX-ROWS maxRows
&Scoped-define SORTBY-PHRASE {&MAX-ROWS}
&Scoped-define maxRows 999999999

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{methods/defines/sortByDefs.i}
{methods/template/brwcustomdef.i}
DEFINE VARIABLE lContinue          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lHeaderSorting     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE dtStartDateTime    AS DATETIME  NO-UNDO.
DEFINE VARIABLE dtEndDateTime      AS DATETIME  NO-UNDO.
DEFINE VARIABLE cAuditKeyFilter    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartAuditKey     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndAuditKey       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAuditKeyFilter    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lBeforeValueFilter AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lAfterValueFilter  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hPgmMstrSecur      AS HANDLE    NO-UNDO.
DEFINE VARIABLE lAdmin             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE dtRecKeyDate       AS DATE      NO-UNDO LABEL "RecKeyDate" FORMAT "99/99/9999".
DEFINE VARIABLE cRecKeyTime        AS CHARACTER NO-UNDO LABEL "RecKeyTime".
DEFINE VARIABLE cStartType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndType           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartUser         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndUser           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartDB           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndDB             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartTable        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndTable          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartField        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndField          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartBeforeValue  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndBeforeValue    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartAfterValue   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndAfterValue     AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdAuditHdrQuery    AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdAuditDtlQuery    AS HANDLE    NO-UNDO.
DEFINE VARIABLE cSortBY            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCurrentBrowse     AS CHARACTER NO-UNDO.

{AOA/tempTable/ttAudit.i}

/* include when using AOA date option objects */
{AOA/includes/dateOptionDef.i}

SESSION:SET-WAIT-STATE("").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME AuditDetail

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES AuditDtl AuditHdr

/* Definitions for BROWSE AuditDetail                                   */
&Scoped-define FIELDS-IN-QUERY-AuditDetail AuditDtl.AuditIdxField ~
AuditDtl.AuditField AuditDtl.AuditExtent AuditDtl.AuditBeforeValue ~
AuditDtl.AuditAfterValue 
&Scoped-define ENABLED-FIELDS-IN-QUERY-AuditDetail 
&Scoped-define QUERY-STRING-AuditDetail FOR EACH AuditDtl ~
      WHERE AuditDtl.AuditID EQ AuditHdr.AuditID ~
AND AuditDtl.AuditField GE cStartField ~
AND AuditDtl.AuditField LE cEndField ~
AND AuditDtl.AuditBeforeValue GE cStartBeforeValue ~
AND AuditDtl.AuditBeforeValue LE cEndBeforeValue ~
AND AuditDtl.AuditAfterValue GE cStartAfterValue ~
AND AuditDtl.AuditAfterValue LE cEndAfterValue NO-LOCK
&Scoped-define OPEN-QUERY-AuditDetail OPEN QUERY AuditDetail FOR EACH AuditDtl ~
      WHERE AuditDtl.AuditID EQ AuditHdr.AuditID ~
AND AuditDtl.AuditField GE cStartField ~
AND AuditDtl.AuditField LE cEndField ~
AND AuditDtl.AuditBeforeValue GE cStartBeforeValue ~
AND AuditDtl.AuditBeforeValue LE cEndBeforeValue ~
AND AuditDtl.AuditAfterValue GE cStartAfterValue ~
AND AuditDtl.AuditAfterValue LE cEndAfterValue NO-LOCK.
&Scoped-define TABLES-IN-QUERY-AuditDetail AuditDtl
&Scoped-define FIRST-TABLE-IN-QUERY-AuditDetail AuditDtl


/* Definitions for BROWSE AuditHeader                                   */
&Scoped-define FIELDS-IN-QUERY-AuditHeader AuditHdr.AuditID AuditHdr.AuditType AuditHdr.AuditDateTime AuditHdr.AuditDB AuditHdr.AuditTable AuditHdr.AuditUser AuditHdr.AuditKey fRecKeyDate(AuditHdr.AuditRecKey) @ dtRecKeyDate fRecKeyTime(AuditHdr.AuditRecKey) @ cRecKeyTime AuditHdr.AuditRecKey AuditHdr.AuditStackID   
&Scoped-define ENABLED-FIELDS-IN-QUERY-AuditHeader   
&Scoped-define SELF-NAME AuditHeader
&Scoped-define QUERY-STRING-AuditHeader FOR EACH AuditHdr NO-LOCK WHERE AuditHdr.AuditDB GE cStartDB AND AuditHdr.AuditDB LE cEndDB AND AuditHdr.AuditTable GE cStartTable AND AuditHdr.AuditTable LE cEndTable AND AuditHdr.AuditDateTime GE dtStartDateTime AND AuditHdr.AuditDateTime LE dtEndDateTime AND AuditHdr.AuditType GE cStartType AND AuditHdr.AuditType LE cEndType AND AuditHdr.AuditUser GE cStartUser AND AuditHdr.AuditUser LE cEndUser AND AuditHdr.AuditKey GE cStartAuditKey AND AuditHdr.AuditKey LE cEndAuditKey, ~
       FIRST AuditDtl OF AuditHdr NO-LOCK WHERE AuditDtl.AuditField GE cStartField AND AuditDtl.AuditField LE cEndField AND AuditDtl.AuditBeforeValue GE cStartBeforeValue AND AuditDtl.AuditBeforeValue LE cEndBeforeValue AND AuditDtl.AuditAfterValue GE cStartAfterValue AND AuditDtl.AuditAfterValue LE cEndAfterValue  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-AuditHeader OPEN QUERY {&SELF-NAME} FOR EACH AuditHdr NO-LOCK WHERE AuditHdr.AuditDB GE cStartDB AND AuditHdr.AuditDB LE cEndDB AND AuditHdr.AuditTable GE cStartTable AND AuditHdr.AuditTable LE cEndTable AND AuditHdr.AuditDateTime GE dtStartDateTime AND AuditHdr.AuditDateTime LE dtEndDateTime AND AuditHdr.AuditType GE cStartType AND AuditHdr.AuditType LE cEndType AND AuditHdr.AuditUser GE cStartUser AND AuditHdr.AuditUser LE cEndUser AND AuditHdr.AuditKey GE cStartAuditKey AND AuditHdr.AuditKey LE cEndAuditKey, ~
       FIRST AuditDtl OF AuditHdr NO-LOCK WHERE AuditDtl.AuditField GE cStartField AND AuditDtl.AuditField LE cEndField AND AuditDtl.AuditBeforeValue GE cStartBeforeValue AND AuditDtl.AuditBeforeValue LE cEndBeforeValue AND AuditDtl.AuditAfterValue GE cStartAfterValue AND AuditDtl.AuditAfterValue LE cEndAfterValue  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-AuditHeader AuditHdr AuditDtl
&Scoped-define FIRST-TABLE-IN-QUERY-AuditHeader AuditHdr
&Scoped-define SECOND-TABLE-IN-QUERY-AuditHeader AuditDtl


/* Definitions for FRAME AuditView                                      */
&Scoped-define FIELDS-IN-QUERY-AuditView AuditHdr.AuditDB ~
AuditHdr.AuditTable AuditDtl.AuditField AuditHdr.AuditType AuditHdr.AuditID ~
AuditHdr.AuditUser AuditHdr.AuditDateTime AuditDtl.AuditIdxField ~
AuditDtl.AuditBeforeValue AuditDtl.AuditExtent AuditDtl.AuditAfterValue 
&Scoped-define QUERY-STRING-AuditView FOR EACH AuditDtl SHARE-LOCK, ~
      EACH AuditHdr OF AuditDtl SHARE-LOCK
&Scoped-define OPEN-QUERY-AuditView OPEN QUERY AuditView FOR EACH AuditDtl SHARE-LOCK, ~
      EACH AuditHdr OF AuditDtl SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-AuditView AuditDtl AuditHdr
&Scoped-define FIRST-TABLE-IN-QUERY-AuditView AuditDtl
&Scoped-define SECOND-TABLE-IN-QUERY-AuditView AuditHdr


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS AuditHeader AuditDetail svSortByHdr ~
svSortByDtl 
&Scoped-Define DISPLAYED-OBJECTS svSortByHdr svSortByDtl 

/* Custom List Definitions                                              */
/* userPrintFields,recKeyFields,recKeyObjects,auditDateFields,searchObjects,List-6 */
&Scoped-define userPrintFields svType svUser svStartDate svStartDateOption ~
svStartDateHour svStartDateMin svEndDate svEndDateOption svEndDateHour ~
svEndDateMin svDB svTable svField maxRows svStartAuditRecKey ~
svEndAuditRecKey svStartRecKeyDate svStartRecKeyDateOption ~
svStartRecKeyHour svStartRecKeyMin svStartRecKeySec svEndRecKeyDate ~
svEndRecKeyDateOption svEndRecKeyHour svEndRecKeyMin svEndRecKeySec 
&Scoped-define recKeyFields svStartAuditRecKey svEndAuditRecKey ~
svStartRecKeyDate svStartRecKeyDateOption svStartRecKeyHour ~
svStartRecKeyMin svStartRecKeySec svEndRecKeyDate svEndRecKeyDateOption ~
svEndRecKeyHour svEndRecKeyMin svEndRecKeySec 
&Scoped-define recKeyObjects btnCalendar-3 btnCalendar-4 
&Scoped-define auditDateFields svStartDate svStartDateHour svEndDate ~
svEndDateHour svEndDateMin 
&Scoped-define searchObjects btnAfterValueFilterClear ~
btnAuditKeyFilterClear btnBeforeValueFilterClear btnFilterAfterValue ~
btnFilterAuditKey btnFilterBeforeValue btnHistory svAuditKeyFilter svType ~
svUser svField svBeforeValueFilter svAfterValueFilter 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fRecKeyDate C-Win 
FUNCTION fRecKeyDate RETURNS DATE
  ( ipcAuditRecKey AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fRecKeyTime C-Win 
FUNCTION fRecKeyTime RETURNS CHARACTER
  ( ipcAuditRecKey AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fSetAuditDate C-Win 
FUNCTION fSetAuditDate RETURNS CHARACTER
  ( ipcType AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fSetAuditRecKey C-Win 
FUNCTION fSetAuditRecKey RETURNS CHARACTER
  ( ipcType AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAfterValueFilterClear 
     IMAGE-UP FILE "Graphics/16x16/move_cross.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1.05 TOOLTIP "Clear After Value Filter".

DEFINE BUTTON btnAuditKeyFilterClear 
     IMAGE-UP FILE "Graphics/16x16/move_cross.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1.05 TOOLTIP "Clear Before Value Filter".

DEFINE BUTTON btnAuditTables 
     IMAGE-UP FILE "Graphics/32x32/document_checks.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8.4 BY 2 TOOLTIP "Select Tables to Audit"
     FONT 4.

DEFINE BUTTON btnBeforeValueFilterClear 
     IMAGE-UP FILE "Graphics/16x16/move_cross.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1.05 TOOLTIP "Clear Before Value Filter".

DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar_clock.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-2 
     IMAGE-UP FILE "Graphics/16x16/calendar_clock.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-3 
     IMAGE-UP FILE "Graphics/16x16/calendar_clock.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-4 
     IMAGE-UP FILE "Graphics/16x16/calendar_clock.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnClear 
     IMAGE-UP FILE "Graphics/32x32/refresh.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Clear" 
     SIZE 8.4 BY 2 TOOLTIP "Clear Filter Values".

DEFINE BUTTON btnFilterAfterValue 
     IMAGE-UP FILE "Graphics/16x16/filterWindow.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Filter" 
     SIZE 4.4 BY 1.05 TOOLTIP "Filter By After Value".

DEFINE BUTTON btnFilterAuditKey 
     IMAGE-UP FILE "Graphics/16x16/filterWindow.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Filter" 
     SIZE 4.4 BY 1.05 TOOLTIP "Filter By Audit Key".

DEFINE BUTTON btnFilterBeforeValue 
     IMAGE-UP FILE "Graphics/16x16/filterWindow.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Filter" 
     SIZE 4.4 BY 1.05 TOOLTIP "Filter By Before Value".

DEFINE BUTTON btnHistory 
     IMAGE-UP FILE "Graphics/16x16/printer.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "History" 
     SIZE 4 BY 1.05 TOOLTIP "View History".

DEFINE BUTTON btnPrint 
     IMAGE-UP FILE "Graphics/32x32/print_New.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8.4 BY 2 TOOLTIP "Print"
     FONT 4.

DEFINE BUTTON btnRestore 
     IMAGE-UP FILE "Graphics/32x32/undo_32.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8.4 BY 2 TOOLTIP "Restore"
     FONT 4.

DEFINE BUTTON btnSearch 
     IMAGE-UP FILE "Graphics/32x32/search-E.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Search" 
     SIZE 8.4 BY 2 TOOLTIP "Search".

DEFINE BUTTON btnStack 
     IMAGE-UP FILE "Graphics/32x32/book_open.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8.4 BY 2 TOOLTIP "Program Trace"
     FONT 4.

DEFINE VARIABLE svDB AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
     LABEL "DB" 
     VIEW-AS COMBO-BOX INNER-LINES 4
     LIST-ITEMS "All" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 TOOLTIP "Select Audit DB Filter" NO-UNDO.

DEFINE VARIABLE svEndDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svEndRecKeyDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svField AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
     LABEL "Field" 
     VIEW-AS COMBO-BOX INNER-LINES 4
     LIST-ITEM-PAIRS "All","All"
     DROP-DOWN-LIST
     SIZE 51 BY 1 TOOLTIP "Select Audit Field Filter" NO-UNDO.

DEFINE VARIABLE svStartDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svStartRecKeyDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svTable AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
     LABEL "Table" 
     VIEW-AS COMBO-BOX INNER-LINES 4
     LIST-ITEM-PAIRS "All","All"
     DROP-DOWN-LIST
     SIZE 51 BY 1 TOOLTIP "Select Audit Table Filter" NO-UNDO.

DEFINE VARIABLE svType AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
     LABEL "Type" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "All" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 TOOLTIP "Select Audit Type Filter" NO-UNDO.

DEFINE VARIABLE svUser AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
     LABEL "User ID" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "All" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 TOOLTIP "Select User Filter" NO-UNDO.

DEFINE VARIABLE initTime AS DECIMAL FORMAT "->>,>>9.999":U INITIAL 0 
     LABEL "Initialize Time" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE maxRows AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 1500 
     LABEL "Max Rows" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE searchTime AS DECIMAL FORMAT "->>,>>9.999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE svAfterValueFilter AS CHARACTER FORMAT "X(256)":U 
     LABEL "After Value" 
     VIEW-AS FILL-IN 
     SIZE 56.4 BY 1 NO-UNDO.

DEFINE VARIABLE svAuditKeyFilter AS CHARACTER FORMAT "X(256)":U 
     LABEL "Audit Key" 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1 NO-UNDO.

DEFINE VARIABLE svBeforeValueFilter AS CHARACTER FORMAT "X(256)":U 
     LABEL "Before Value" 
     VIEW-AS FILL-IN 
     SIZE 56.4 BY 1 NO-UNDO.

DEFINE VARIABLE svEndAuditRecKey AS CHARACTER FORMAT "X(256)":U 
     LABEL "End Rec Key" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE svEndDate AS DATE FORMAT "99/99/9999":U INITIAL 12/31/49 
     LABEL "To Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Enter To Date" NO-UNDO.

DEFINE VARIABLE svEndDateHour AS INTEGER FORMAT "99":U INITIAL 23 
     LABEL "Time" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE svEndDateMin AS INTEGER FORMAT "99":U INITIAL 59 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE svEndRecKeyDate AS DATE FORMAT "99/99/9999":U INITIAL 12/31/49 
     LABEL "To RecKey  Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Enter To Date" NO-UNDO.

DEFINE VARIABLE svEndRecKeyHour AS INTEGER FORMAT "99":U INITIAL 23 
     LABEL "End Time" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE svEndRecKeyMin AS INTEGER FORMAT "99":U INITIAL 59 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE svEndRecKeySec AS INTEGER FORMAT "99":U INITIAL 59 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE svStartAuditRecKey AS CHARACTER FORMAT "X(256)":U 
     LABEL "Start Rec Key" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE svStartDate AS DATE FORMAT "99/99/9999":U INITIAL 01/01/50 
     LABEL "From Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Enter From Date" NO-UNDO.

DEFINE VARIABLE svStartDateHour AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Time" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE svStartDateMin AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE svStartRecKeyDate AS DATE FORMAT "99/99/9999":U INITIAL 01/01/50 
     LABEL "From RecKey Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Enter From Date" NO-UNDO.

DEFINE VARIABLE svStartRecKeyHour AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Start Time" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE svStartRecKeyMin AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE svStartRecKeySec AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE svUseRecKeySearch AS LOGICAL INITIAL no 
     LABEL "Use Rec Key Search" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE svSortByDtl AS CHARACTER FORMAT "X(256)":U 
     LABEL " Sorted By" 
      VIEW-AS TEXT 
     SIZE 25 BY .62 NO-UNDO.

DEFINE VARIABLE svSortByHdr AS CHARACTER FORMAT "X(256)":U 
     LABEL " Sorted By" 
      VIEW-AS TEXT 
     SIZE 26 BY .62 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY AuditDetail FOR 
      AuditDtl SCROLLING.

DEFINE QUERY AuditHeader FOR 
      AuditHdr, 
      AuditDtl SCROLLING.

DEFINE QUERY AuditView FOR 
      AuditDtl, 
      AuditHdr SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE AuditDetail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS AuditDetail C-Win _STRUCTURED
  QUERY AuditDetail NO-LOCK DISPLAY
      AuditDtl.AuditIdxField FORMAT "-X-/":U LABEL-BGCOLOR 14
      AuditDtl.AuditField FORMAT "x(16)":U LABEL-BGCOLOR 14
      AuditDtl.AuditExtent FORMAT ">>>9":U LABEL-BGCOLOR 14
      AuditDtl.AuditBeforeValue FORMAT "x(45)":U LABEL-BGCOLOR 14
      AuditDtl.AuditAfterValue FORMAT "x(45)":U LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 124 BY 15.24
         FGCOLOR 1 
         TITLE FGCOLOR 1 "Audit Detail".

DEFINE BROWSE AuditHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS AuditHeader C-Win _FREEFORM
  QUERY AuditHeader NO-LOCK DISPLAY
      AuditHdr.AuditID FORMAT ">,>>>,>>>,>>9":U LABEL-BGCOLOR 14
AuditHdr.AuditType FORMAT "x(8)":U WIDTH 11 LABEL-BGCOLOR 14
AuditHdr.AuditDateTime COLUMN-LABEL "Date / Time" FORMAT "99/99/9999 HH:MM:SS.SSS":U
      LABEL-BGCOLOR 14
AuditHdr.AuditDB COLUMN-LABEL "DB" FORMAT "x(6)":U LABEL-BGCOLOR 14
AuditHdr.AuditTable COLUMN-LABEL "Table / Prgm" FORMAT "x(16)":U
      LABEL-BGCOLOR 14
AuditHdr.AuditUser FORMAT "x(16)":U LABEL-BGCOLOR 14
AuditHdr.AuditKey FORMAT "x(40)":U LABEL-BGCOLOR 14
fRecKeyDate(AuditHdr.AuditRecKey) @ dtRecKeyDate
fRecKeyTime(AuditHdr.AuditRecKey) @ cRecKeyTime
AuditHdr.AuditRecKey FORMAT "x(25)":U LABEL-BGCOLOR 14
AuditHdr.AuditStackID FORMAT "->,>>>,>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 114 BY 21.43
         FGCOLOR 1 
         TITLE FGCOLOR 1 "Audit Header".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     AuditHeader AT ROW 8.14 COL 1 WIDGET-ID 200
     AuditDetail AT ROW 8.14 COL 115 WIDGET-ID 300
     svSortByHdr AT ROW 8.38 COL 86 COLON-ALIGNED WIDGET-ID 2
     svSortByDtl AT ROW 8.38 COL 211 COLON-ALIGNED WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 238.2 BY 28.57
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.

DEFINE FRAME AuditView
     AuditHdr.AuditDB AT ROW 1.24 COL 6 COLON-ALIGNED WIDGET-ID 16
          LABEL "DB"
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
          BGCOLOR 15 
     AuditHdr.AuditTable AT ROW 1.24 COL 30 COLON-ALIGNED WIDGET-ID 20
          LABEL "Table"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     AuditDtl.AuditField AT ROW 1.24 COL 58 COLON-ALIGNED WIDGET-ID 8
          LABEL "Field"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     AuditHdr.AuditType AT ROW 2.43 COL 6 COLON-ALIGNED WIDGET-ID 22
          LABEL "Type"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
          BGCOLOR 15 
     AuditHdr.AuditID AT ROW 2.43 COL 30 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
          BGCOLOR 15 
     AuditHdr.AuditUser AT ROW 2.43 COL 58 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
          BGCOLOR 15 
     AuditHdr.AuditDateTime AT ROW 2.43 COL 88 COLON-ALIGNED WIDGET-ID 14
          LABEL "Date/Time"
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
          BGCOLOR 15 
     AuditDtl.AuditIdxField AT ROW 3.62 COL 14 COLON-ALIGNED WIDGET-ID 12
          LABEL "Idx" FORMAT "Yes/No"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
          BGCOLOR 15 
     AuditDtl.AuditBeforeValue AT ROW 3.62 COL 30 COLON-ALIGNED WIDGET-ID 4
          LABEL "Before" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 92 BY 1
          BGCOLOR 15 
     AuditDtl.AuditExtent AT ROW 4.81 COL 14 COLON-ALIGNED WIDGET-ID 6
          LABEL "Ext"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
          BGCOLOR 15 
     AuditDtl.AuditAfterValue AT ROW 4.81 COL 30 COLON-ALIGNED WIDGET-ID 2
          LABEL "After" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 92 BY 1
          BGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 115 ROW 23.38
         SIZE 124 BY 6
         BGCOLOR 15 FGCOLOR 1 
         TITLE BGCOLOR 15 "Audit Detail View" WIDGET-ID 400.

DEFINE FRAME AuditSearch
     btnAfterValueFilterClear AT ROW 3.62 COL 234 HELP
          "Click to Clear After Value Filter" WIDGET-ID 42
     btnAuditKeyFilterClear AT ROW 1.24 COL 234 HELP
          "Click to Clear Before Value Filter" WIDGET-ID 346
     btnBeforeValueFilterClear AT ROW 2.43 COL 234 HELP
          "Click to Clear Before Value Filter" WIDGET-ID 40
     btnCalendar-1 AT ROW 1.24 COL 55 WIDGET-ID 272
     btnCalendar-2 AT ROW 2.43 COL 55 WIDGET-ID 274
     btnCalendar-3 AT ROW 4.81 COL 104 WIDGET-ID 304
     btnCalendar-4 AT ROW 6 COL 104 WIDGET-ID 306
     btnFilterAfterValue AT ROW 3.62 COL 229 HELP
          "Select to Filter by After Value" WIDGET-ID 34
     btnFilterAuditKey AT ROW 1.24 COL 229 HELP
          "Select to Filter by Audit Key" WIDGET-ID 28
     btnFilterBeforeValue AT ROW 2.43 COL 229 HELP
          "Select to Filter by Before Value" WIDGET-ID 32
     btnHistory AT ROW 4.81 COL 234.2 HELP
          "Click to View History" WIDGET-ID 30
     svAuditKeyFilter AT ROW 1.24 COL 171 COLON-ALIGNED WIDGET-ID 344
     svType AT ROW 1.24 COL 8 COLON-ALIGNED HELP
          "Select Audit Type Filter" WIDGET-ID 6
     svUser AT ROW 2.43 COL 8 COLON-ALIGNED HELP
          "Select User Filter" WIDGET-ID 12
     btnRestore AT ROW 5.05 COL 2 HELP
          "Print" WIDGET-ID 320
     svStartDate AT ROW 1.24 COL 36 COLON-ALIGNED HELP
          "Enter From Date" WIDGET-ID 20
     svStartDateOption AT ROW 1.24 COL 59 HELP
          "Select Start Date Option" NO-LABEL WIDGET-ID 74
     svStartDateHour AT ROW 1.24 COL 89 COLON-ALIGNED HELP
          "Enter Start Date Hour" WIDGET-ID 338
     svStartDateMin AT ROW 1.24 COL 94 COLON-ALIGNED HELP
          "Enter Start Date Minute" NO-LABEL WIDGET-ID 340
     svEndDate AT ROW 2.43 COL 36 COLON-ALIGNED HELP
          "Enter To Date" WIDGET-ID 22
     svEndDateOption AT ROW 2.43 COL 59 HELP
          "Select End Date Option" NO-LABEL WIDGET-ID 70
     svEndDateHour AT ROW 2.43 COL 89 COLON-ALIGNED HELP
          "Enter End Date Hour" WIDGET-ID 334
     svEndDateMin AT ROW 2.43 COL 94 COLON-ALIGNED HELP
          "Enter End Date Minute" NO-LABEL WIDGET-ID 336
     svDB AT ROW 1.24 COL 106 COLON-ALIGNED HELP
          "Select Audit DB Filter" WIDGET-ID 14
     svTable AT ROW 2.43 COL 106 COLON-ALIGNED HELP
          "Select Audit Table Filter" WIDGET-ID 16
     svField AT ROW 3.62 COL 106 COLON-ALIGNED HELP
          "Select Audit Field Filter" WIDGET-ID 18
     maxRows AT ROW 1.24 COL 144 COLON-ALIGNED WIDGET-ID 290
     btnPrint AT ROW 5.05 COL 197 HELP
          "Print" WIDGET-ID 280
     svBeforeValueFilter AT ROW 2.43 COL 171 COLON-ALIGNED HELP
          "Enter Before Value to Filter" WIDGET-ID 36
     svAfterValueFilter AT ROW 3.62 COL 171 COLON-ALIGNED HELP
          "Enter After Value to Filter" WIDGET-ID 38
     svUseRecKeySearch AT ROW 3.62 COL 10 WIDGET-ID 342
     svStartAuditRecKey AT ROW 4.81 COL 36 COLON-ALIGNED HELP
          "Enter Start Audit Table Rec Key Value" WIDGET-ID 298
     svEndAuditRecKey AT ROW 6 COL 36 COLON-ALIGNED HELP
          "Enter End Audit Table Rec Key Value" WIDGET-ID 326
     svStartRecKeyDate AT ROW 4.81 COL 85 COLON-ALIGNED HELP
          "Enter From Date" WIDGET-ID 300
     svStartRecKeyDateOption AT ROW 4.81 COL 108 HELP
          "Select Start Date Option" NO-LABEL WIDGET-ID 310
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 238 BY 7.14
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 500.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME AuditSearch
     svStartRecKeyHour AT ROW 4.81 COL 143 COLON-ALIGNED HELP
          "Enter Start Rec Key Hour" WIDGET-ID 312
     svStartRecKeyMin AT ROW 4.81 COL 148 COLON-ALIGNED HELP
          "Enter Start Rec Key Minute" NO-LABEL WIDGET-ID 314
     svStartRecKeySec AT ROW 4.81 COL 153 COLON-ALIGNED HELP
          "Enter Start Rec Key Second" NO-LABEL WIDGET-ID 328
     svEndRecKeyDate AT ROW 6 COL 85 COLON-ALIGNED HELP
          "Enter To Date" WIDGET-ID 302
     svEndRecKeyDateOption AT ROW 6 COL 108 HELP
          "Select End Date Option" NO-LABEL WIDGET-ID 308
     svEndRecKeyHour AT ROW 6 COL 143 COLON-ALIGNED HELP
          "Enter End Rec Key Hour" WIDGET-ID 316
     svEndRecKeyMin AT ROW 6 COL 148 COLON-ALIGNED HELP
          "Enter End Rec Key Minute" NO-LABEL WIDGET-ID 318
     svEndRecKeySec AT ROW 6 COL 153 COLON-ALIGNED HELP
          "Enter End Rec Key Second" NO-LABEL WIDGET-ID 330
     searchTime AT ROW 6 COL 158 COLON-ALIGNED NO-LABEL WIDGET-ID 324
     initTime AT ROW 3.62 COL 85 COLON-ALIGNED WIDGET-ID 322
     btnStack AT ROW 5.05 COL 209 HELP
          "Click to View Program Stack Trace" WIDGET-ID 282
     btnAuditTables AT ROW 5.05 COL 221 HELP
          "Click to Access Tables to Audit" WIDGET-ID 288
     btnClear AT ROW 5.05 COL 185 HELP
          "Click to Clear Filters" WIDGET-ID 284
     btnSearch AT ROW 5.05 COL 173 HELP
          "Click to Apply Filter Selections" WIDGET-ID 286
     "Search Time" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 5.29 COL 160 WIDGET-ID 332
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 238 BY 7.14
         BGCOLOR 15 FGCOLOR 1 
         TITLE "Search Filters" WIDGET-ID 500.


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
         TITLE              = "Audit History"
         HEIGHT             = 28.57
         WIDTH              = 238.2
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 238.2
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 238.2
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
ASSIGN FRAME AuditSearch:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME AuditView:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME AuditSearch
   Custom                                                               */
/* SETTINGS FOR BUTTON btnAfterValueFilterClear IN FRAME AuditSearch
   5                                                                    */
/* SETTINGS FOR BUTTON btnAuditKeyFilterClear IN FRAME AuditSearch
   5                                                                    */
/* SETTINGS FOR BUTTON btnBeforeValueFilterClear IN FRAME AuditSearch
   5                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-3 IN FRAME AuditSearch
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-4 IN FRAME AuditSearch
   3                                                                    */
/* SETTINGS FOR BUTTON btnFilterAfterValue IN FRAME AuditSearch
   5                                                                    */
/* SETTINGS FOR BUTTON btnFilterAuditKey IN FRAME AuditSearch
   5                                                                    */
/* SETTINGS FOR BUTTON btnFilterBeforeValue IN FRAME AuditSearch
   5                                                                    */
/* SETTINGS FOR BUTTON btnHistory IN FRAME AuditSearch
   5                                                                    */
/* SETTINGS FOR BUTTON btnRestore IN FRAME AuditSearch
   NO-ENABLE                                                            */
ASSIGN 
       btnRestore:HIDDEN IN FRAME AuditSearch           = TRUE.

/* SETTINGS FOR FILL-IN initTime IN FRAME AuditSearch
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN maxRows IN FRAME AuditSearch
   1                                                                    */
ASSIGN 
       maxRows:PRIVATE-DATA IN FRAME AuditSearch     = 
                "NoUserPrint".

/* SETTINGS FOR FILL-IN searchTime IN FRAME AuditSearch
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN svAfterValueFilter IN FRAME AuditSearch
   5                                                                    */
ASSIGN 
       svAfterValueFilter:PRIVATE-DATA IN FRAME AuditSearch     = 
                "parameter".

/* SETTINGS FOR FILL-IN svAuditKeyFilter IN FRAME AuditSearch
   5                                                                    */
/* SETTINGS FOR FILL-IN svBeforeValueFilter IN FRAME AuditSearch
   5                                                                    */
ASSIGN 
       svBeforeValueFilter:PRIVATE-DATA IN FRAME AuditSearch     = 
                "parameter".

/* SETTINGS FOR COMBO-BOX svDB IN FRAME AuditSearch
   1                                                                    */
ASSIGN 
       svDB:PRIVATE-DATA IN FRAME AuditSearch     = 
                "parameter".

/* SETTINGS FOR FILL-IN svEndAuditRecKey IN FRAME AuditSearch
   1 2                                                                  */
/* SETTINGS FOR FILL-IN svEndDate IN FRAME AuditSearch
   1 4                                                                  */
ASSIGN 
       svEndDate:PRIVATE-DATA IN FRAME AuditSearch     = 
                "parameter".

/* SETTINGS FOR FILL-IN svEndDateHour IN FRAME AuditSearch
   1 4                                                                  */
/* SETTINGS FOR FILL-IN svEndDateMin IN FRAME AuditSearch
   1 4                                                                  */
/* SETTINGS FOR COMBO-BOX svEndDateOption IN FRAME AuditSearch
   ALIGN-L 1                                                            */
ASSIGN 
       svEndDateOption:PRIVATE-DATA IN FRAME AuditSearch     = 
                "parameter".

/* SETTINGS FOR FILL-IN svEndRecKeyDate IN FRAME AuditSearch
   1 2                                                                  */
ASSIGN 
       svEndRecKeyDate:PRIVATE-DATA IN FRAME AuditSearch     = 
                "parameter".

/* SETTINGS FOR COMBO-BOX svEndRecKeyDateOption IN FRAME AuditSearch
   ALIGN-L 1 2                                                          */
ASSIGN 
       svEndRecKeyDateOption:PRIVATE-DATA IN FRAME AuditSearch     = 
                "parameter".

/* SETTINGS FOR FILL-IN svEndRecKeyHour IN FRAME AuditSearch
   1 2                                                                  */
/* SETTINGS FOR FILL-IN svEndRecKeyMin IN FRAME AuditSearch
   1 2                                                                  */
/* SETTINGS FOR FILL-IN svEndRecKeySec IN FRAME AuditSearch
   1 2                                                                  */
/* SETTINGS FOR COMBO-BOX svField IN FRAME AuditSearch
   1 5                                                                  */
ASSIGN 
       svField:PRIVATE-DATA IN FRAME AuditSearch     = 
                "parameter".

/* SETTINGS FOR FILL-IN svStartAuditRecKey IN FRAME AuditSearch
   1 2                                                                  */
/* SETTINGS FOR FILL-IN svStartDate IN FRAME AuditSearch
   1 4                                                                  */
ASSIGN 
       svStartDate:PRIVATE-DATA IN FRAME AuditSearch     = 
                "parameter".

/* SETTINGS FOR FILL-IN svStartDateHour IN FRAME AuditSearch
   1 4                                                                  */
/* SETTINGS FOR FILL-IN svStartDateMin IN FRAME AuditSearch
   1                                                                    */
/* SETTINGS FOR COMBO-BOX svStartDateOption IN FRAME AuditSearch
   ALIGN-L 1                                                            */
ASSIGN 
       svStartDateOption:PRIVATE-DATA IN FRAME AuditSearch     = 
                "parameter".

/* SETTINGS FOR FILL-IN svStartRecKeyDate IN FRAME AuditSearch
   1 2                                                                  */
ASSIGN 
       svStartRecKeyDate:PRIVATE-DATA IN FRAME AuditSearch     = 
                "parameter".

/* SETTINGS FOR COMBO-BOX svStartRecKeyDateOption IN FRAME AuditSearch
   ALIGN-L 1 2                                                          */
ASSIGN 
       svStartRecKeyDateOption:PRIVATE-DATA IN FRAME AuditSearch     = 
                "parameter".

/* SETTINGS FOR FILL-IN svStartRecKeyHour IN FRAME AuditSearch
   1 2                                                                  */
/* SETTINGS FOR FILL-IN svStartRecKeyMin IN FRAME AuditSearch
   1 2                                                                  */
/* SETTINGS FOR FILL-IN svStartRecKeySec IN FRAME AuditSearch
   1 2                                                                  */
/* SETTINGS FOR COMBO-BOX svTable IN FRAME AuditSearch
   1                                                                    */
ASSIGN 
       svTable:PRIVATE-DATA IN FRAME AuditSearch     = 
                "parameter".

/* SETTINGS FOR COMBO-BOX svType IN FRAME AuditSearch
   1 5                                                                  */
ASSIGN 
       svType:PRIVATE-DATA IN FRAME AuditSearch     = 
                "parameter".

/* SETTINGS FOR COMBO-BOX svUser IN FRAME AuditSearch
   1 5                                                                  */
ASSIGN 
       svUser:PRIVATE-DATA IN FRAME AuditSearch     = 
                "parameter".

/* SETTINGS FOR FRAME AuditView
                                                                        */
/* SETTINGS FOR FILL-IN AuditDtl.AuditAfterValue IN FRAME AuditView
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN AuditDtl.AuditBeforeValue IN FRAME AuditView
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN AuditHdr.AuditDateTime IN FRAME AuditView
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN AuditHdr.AuditDB IN FRAME AuditView
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN AuditDtl.AuditExtent IN FRAME AuditView
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN AuditDtl.AuditField IN FRAME AuditView
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN AuditHdr.AuditID IN FRAME AuditView
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AuditDtl.AuditIdxField IN FRAME AuditView
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN AuditHdr.AuditTable IN FRAME AuditView
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN AuditHdr.AuditType IN FRAME AuditView
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN AuditHdr.AuditUser IN FRAME AuditView
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME AuditSearch:MOVE-BEFORE-TAB-ITEM (AuditHeader:HANDLE IN FRAME DEFAULT-FRAME)
       XXTABVALXX = FRAME AuditView:MOVE-AFTER-TAB-ITEM (AuditDetail:HANDLE IN FRAME DEFAULT-FRAME)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB AuditHeader AuditSearch DEFAULT-FRAME */
/* BROWSE-TAB AuditDetail AuditHeader DEFAULT-FRAME */
ASSIGN 
       AuditDetail:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       AuditHeader:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 3
       AuditHeader:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       svSortByDtl:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       svSortByHdr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE AuditDetail
/* Query rebuild information for BROWSE AuditDetail
     _TblList          = "audit.AuditDtl"
     _Options          = "NO-LOCK"
     _Where[1]         = "AuditDtl.AuditID EQ AuditHdr.AuditID
AND AuditDtl.AuditField GE cStartField
AND AuditDtl.AuditField LE cEndField
AND AuditDtl.AuditBeforeValue GE cStartBeforeValue
AND AuditDtl.AuditBeforeValue LE cEndBeforeValue
AND AuditDtl.AuditAfterValue GE cStartAfterValue
AND AuditDtl.AuditAfterValue LE cEndAfterValue"
     _FldNameList[1]   > audit.AuditDtl.AuditIdxField
"AuditDtl.AuditIdxField" ? "-X-/" "logical" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > audit.AuditDtl.AuditField
"AuditDtl.AuditField" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > audit.AuditDtl.AuditExtent
"AuditDtl.AuditExtent" ? ? "integer" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > audit.AuditDtl.AuditBeforeValue
"AuditDtl.AuditBeforeValue" ? "x(45)" "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > audit.AuditDtl.AuditAfterValue
"AuditDtl.AuditAfterValue" ? "x(45)" "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE AuditDetail */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE AuditHeader
/* Query rebuild information for BROWSE AuditHeader
     _START_FREEFORM
OPEN QUERY {&SELF-NAME}
FOR EACH AuditHdr NO-LOCK
WHERE AuditHdr.AuditDB GE cStartDB
AND AuditHdr.AuditDB LE cEndDB
AND AuditHdr.AuditTable GE cStartTable
AND AuditHdr.AuditTable LE cEndTable
AND AuditHdr.AuditDateTime GE dtStartDateTime
AND AuditHdr.AuditDateTime LE dtEndDateTime
AND AuditHdr.AuditType GE cStartType
AND AuditHdr.AuditType LE cEndType
AND AuditHdr.AuditUser GE cStartUser
AND AuditHdr.AuditUser LE cEndUser
AND AuditHdr.AuditKey GE cStartAuditKey
AND AuditHdr.AuditKey LE cEndAuditKey,
FIRST AuditDtl OF AuditHdr NO-LOCK
WHERE AuditDtl.AuditField GE cStartField
AND AuditDtl.AuditField LE cEndField
AND AuditDtl.AuditBeforeValue GE cStartBeforeValue
AND AuditDtl.AuditBeforeValue LE cEndBeforeValue
AND AuditDtl.AuditAfterValue GE cStartAfterValue
AND AuditDtl.AuditAfterValue LE cEndAfterValue
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK SORTBY-PHRASE"
     _TblOptList       = ", EACH, FIRST"
     _Where[1]         = "AuditHdr.AuditDB GE cStartDB
AND AuditHdr.AuditDB LE cEndDB
AND AuditHdr.AuditTable GE cStartTable
AND AuditHdr.AuditTable LE cEndTable
AND AuditHdr.AuditDateTime GE dtStartDateTime
AND AuditHdr.AuditDateTime LE dtEndDateTime
AND AuditHdr.AuditType GE cStartType
AND AuditHdr.AuditType LE cEndType
AND AuditHdr.AuditUser GE cStartUser
AND AuditHdr.AuditUser LE cEndUser
AND AuditHdr.AuditKey GE cStartAuditKey
AND AuditHdr.AuditKey LE cEndAuditKey
"
     _Where[2]         = "AuditDtl.AuditField GE cStartField
AND AuditDtl.AuditField LE cEndField
AND AuditDtl.AuditBeforeValue GE cStartBeforeValue
AND AuditDtl.AuditBeforeValue LE cEndBeforeValue
AND AuditDtl.AuditAfterValue GE cStartAfterValue
AND AuditDtl.AuditAfterValue LE cEndAfterValue"
     _Query            is NOT OPENED
*/  /* BROWSE AuditHeader */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME AuditSearch
/* Query rebuild information for FRAME AuditSearch
     _Query            is NOT OPENED
*/  /* FRAME AuditSearch */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME AuditView
/* Query rebuild information for FRAME AuditView
     _TblList          = "audit.AuditDtl,audit.AuditHdr OF audit.AuditDtl"
     _Query            is NOT OPENED
*/  /* FRAME AuditView */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Audit History */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Audit History */
DO:
  /* This event will close the window and terminate the procedure.  */
  &IF DEFINED(AuditHistory) EQ 0 &THEN
  RUN pSaveSettings.
  &ENDIF
  IF VALID-HANDLE(hdAuditHdrQuery) THEN 
      DELETE OBJECT hdAuditHdrQuery.
  IF VALID-HANDLE(hdAuditDtlQuery) THEN   
      DELETE OBJECT hdAuditDtlQuery.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Audit History */
DO:
  RUN pWinReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME AuditDetail
&Scoped-define SELF-NAME AuditDetail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AuditDetail C-Win
ON START-SEARCH OF AuditDetail IN FRAME DEFAULT-FRAME /* Audit Detail */
DO:
        {methods/template/sortindicator.i} 
    IF {&BROWSE-NAME}:CURRENT-COLUMN:NAME NE ? THEN DO:
        cColumnLabel = BROWSE AuditDetail:CURRENT-COLUMN:NAME.
        IF cColumnLabel EQ cSaveLabel THEN
        lAscending = NOT lAscending.
        ASSIGN
            lHeaderSorting = NO
            cSaveLabel = cColumnLabel
            svSortByDtl:SCREEN-VALUE = BROWSE AuditDetail:CURRENT-COLUMN:LABEL + " "
                                     + STRING(lAscending,"Ascending/Descending")
            cCurrentBrowse = {&BROWSE-NAME}:NAME
            .
        RUN pReopenBrowse.
    END.
        {methods/template/sortindicatorend.i} 
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AuditDetail C-Win
ON VALUE-CHANGED OF AuditDetail IN FRAME DEFAULT-FRAME /* Audit Detail */
DO:
    IF AVAILABLE AuditDtl THEN DO:
        DISPLAY
            AuditHdr.AuditID
            AuditHdr.AuditType
            AuditHdr.AuditDateTime
            AuditHdr.AuditUser
            AuditHdr.AuditDB
            AuditHdr.AuditTable
            AuditDtl.AuditField
            AuditDtl.AuditExtent
            AuditDtl.AuditIdxField
            AuditDtl.AuditBeforeValue
            AuditDtl.AuditAfterValue
                WITH FRAME AuditView.
    END. /* if avail */
    ELSE
        MESSAGE
            "No Audit History Records Found."
        VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME AuditHeader
&Scoped-define SELF-NAME AuditHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AuditHeader C-Win
ON START-SEARCH OF AuditHeader IN FRAME DEFAULT-FRAME /* Audit Header */
DO:
        {methods/template/sortindicator.i} 
    IF {&BROWSE-NAME}:CURRENT-COLUMN:NAME NE ? THEN DO:
        cColumnLabel = BROWSE AuditHeader:CURRENT-COLUMN:NAME.
        IF cColumnLabel EQ cSaveLabel THEN
        lAscending = NOT lAscending.
        ASSIGN
            lHeaderSorting = YES
            cSaveLabel = cColumnLabel
            svSortByHdr:SCREEN-VALUE = BROWSE AuditHeader:CURRENT-COLUMN:LABEL + " "
                                     + STRING(lAscending,"Ascending/Descending")
            cCurrentBrowse = {&BROWSE-NAME}:NAME
            .
        RUN pReopenBrowse.
    END.
        {methods/template/sortindicatorend.i} 
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AuditHeader C-Win
ON VALUE-CHANGED OF AuditHeader IN FRAME DEFAULT-FRAME /* Audit Header */
DO:
    svSortByDtl:SCREEN-VALUE = "".
    IF lAdmin AND AVAILABLE AuditHdr THEN /* admin security level */
    ASSIGN
        btnRestore:HIDDEN IN FRAME AuditSearch    = AuditHdr.AuditType NE "DELETE"
        btnRestore:SENSITIVE = AuditHdr.AuditType EQ "DELETE"
        .
    RUN pPrepareAndExecuteQueryForDetail.
    APPLY "VALUE-CHANGED":U TO BROWSE AuditDetail.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME AuditSearch
&Scoped-define SELF-NAME btnAfterValueFilterClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAfterValueFilterClear C-Win
ON CHOOSE OF btnAfterValueFilterClear IN FRAME AuditSearch
DO:
    ASSIGN
        svAfterValueFilter:SCREEN-VALUE = ""
        svAfterValueFilter
        lAfterValueFilter = NO
        cStartAfterValue  = CHR(32)
        cEndAfterValue    = CHR(254)
        svAfterValueFilter:BGCOLOR = 15
        .
/*    APPLY "CHOOSE":U TO btnSearch.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAuditKeyFilterClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAuditKeyFilterClear C-Win
ON CHOOSE OF btnAuditKeyFilterClear IN FRAME AuditSearch
DO:
    ASSIGN
        svAuditKeyFilter:SCREEN-VALUE = ""
        svAuditKeyFilter
        lAuditKeyFilter = NO
        cStartAuditKey  = CHR(32)
        cEndAuditKey    = CHR(254)
        svAuditKeyFilter:BGCOLOR = 15
        .
/*    APPLY "CHOOSE":U TO btnSearch.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAuditTables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAuditTables C-Win
ON CHOOSE OF btnAuditTables IN FRAME AuditSearch
DO:
    RUN nosweat/setaudit.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBeforeValueFilterClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBeforeValueFilterClear C-Win
ON CHOOSE OF btnBeforeValueFilterClear IN FRAME AuditSearch
DO:
    ASSIGN
        svBeforeValueFilter:SCREEN-VALUE = ""
        svBeforeValueFilter
        lBeforeValueFilter = NO
        cStartBeforeValue  = CHR(32)
        cEndBeforeValue    = CHR(254)
        svBeforeValueFilter:BGCOLOR = 15
        .
/*    APPLY "CHOOSE":U TO btnSearch.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 C-Win
ON CHOOSE OF btnCalendar-1 IN FRAME AuditSearch
DO:
    {methods/btnCalendar.i svStartDate}
    APPLY "LEAVE":U TO svStartDate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 C-Win
ON CHOOSE OF btnCalendar-2 IN FRAME AuditSearch
DO:
    {methods/btnCalendar.i svEndDate}
    APPLY "LEAVE":U TO svEndDate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-3 C-Win
ON CHOOSE OF btnCalendar-3 IN FRAME AuditSearch
DO:
    {methods/btnCalendar.i svStartRecKeyDate}
    APPLY "LEAVE":U TO svStartRecKeyDate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-4 C-Win
ON CHOOSE OF btnCalendar-4 IN FRAME AuditSearch
DO:
    {methods/btnCalendar.i svEndRecKeyDate}
    APPLY "LEAVE":U TO svEndRecKeyDate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClear C-Win
ON CHOOSE OF btnClear IN FRAME AuditSearch /* Clear */
DO:
    ASSIGN
        {&WINDOW-NAME}:TITLE        = "Audit History"
        svType:SCREEN-VALUE         = "All"
        svType
        svUser:SCREEN-VALUE         = "All"
        svUser
        svDB:SCREEN-VALUE           = "All"
        svDB
        svTable:SCREEN-VALUE        = "All"
        svTable
        svField:SCREEN-VALUE        = "All"
        svField
        lAuditKeyFilter             = NO
        cStartAuditKey              = CHR(32)
        cEndAuditKey                = CHR(254)
        svAuditKeyFilter:BGCOLOR    = 15
        lBeforeValueFilter          = NO
        cStartBeforeValue           = CHR(32)
        cEndBeforeValue             = CHR(254)
        svBeforeValueFilter:BGCOLOR = 15
        lAfterValueFilter           = NO
        cStartAfterValue            = CHR(32)
        cEndAfterValue              = CHR(254)
        svAfterValueFilter:BGCOLOR  = 15
        .
    RUN pGetFilterValues ("ALL").
    RUN pGetFilterValues ("TABLE").
    RUN pGetFilterValues ("FIELD").
    APPLY "VALUE-CHANGED":U TO svStartDateOption.
    APPLY "VALUE-CHANGED":U TO svEndDateOption.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFilterAfterValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFilterAfterValue C-Win
ON CHOOSE OF btnFilterAfterValue IN FRAME AuditSearch /* Filter */
DO:
    ASSIGN
        svAfterValueFilter
        lAfterValueFilter = NOT lAfterValueFilter
        svAfterValueFilter:BGCOLOR = IF lAfterValueFilter THEN 14 ELSE 15
        .
    RUN pSetStartEndRange (svAfterValueFilter:HANDLE).
/*    APPLY "CHOOSE":U TO btnSearch.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFilterAuditKey
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFilterAuditKey C-Win
ON CHOOSE OF btnFilterAuditKey IN FRAME AuditSearch /* Filter */
DO:
    ASSIGN
        svAuditKeyFilter
        lAuditKeyFilter = NOT lAuditKeyFilter
        svAuditKeyFilter:BGCOLOR = IF lAuditKeyFilter THEN 14 ELSE 15
        .
    RUN pSetStartEndRange (svAuditKeyFilter:HANDLE).
/*    APPLY "CHOOSE":U TO btnSearch.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFilterBeforeValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFilterBeforeValue C-Win
ON CHOOSE OF btnFilterBeforeValue IN FRAME AuditSearch /* Filter */
DO:
    ASSIGN
        svBeforeValueFilter
        lBeforeValueFilter = NOT lBeforeValueFilter
        svBeforeValueFilter:BGCOLOR = IF lBeforeValueFilter THEN 14 ELSE 15
        .
    RUN pSetStartEndRange (svBeforeValueFilter:HANDLE).
/*    APPLY "CHOOSE":U TO btnSearch.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnHistory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHistory C-Win
ON CHOOSE OF btnHistory IN FRAME AuditSearch /* History */
DO:
    RUN pHistory (AuditHdr.AuditKey).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrint C-Win
ON CHOOSE OF btnPrint IN FRAME AuditSearch
DO:
    RUN pUserPrint.
    RUN AOA/Jasper.p (11,USERID('ASI'),"",0,YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRestore
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRestore C-Win
ON CHOOSE OF btnRestore IN FRAME AuditSearch
DO:
    RUN pRestore.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSearch C-Win
ON CHOOSE OF btnSearch IN FRAME AuditSearch /* Search */
DO:
    ASSIGN
        {&userPrintFields}
        svSortByHdr:SCREEN-VALUE IN FRAME DEFAULT-FRAME = ""
        svSortByDtl:SCREEN-VALUE = ""
        lHeaderSorting = YES
        cColumnLabel   = ""
        cSaveLabel     = ""
        cSortBy        = ""
        cCurrentBrowse = "AuditHeader"
        .
        
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStack
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStack C-Win
ON CHOOSE OF btnStack IN FRAME AuditSearch
DO:
    IF AVAILABLE AuditHdr THEN
    RUN system\AuditStack.w (AuditHdr.AuditStackID).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME maxRows
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL maxRows C-Win
ON VALUE-CHANGED OF maxRows IN FRAME AuditSearch /* Max Rows */
DO:
  ASSIGN {&SELF-NAME}.
  IF {&SELF-NAME} GT {&maxRows} THEN
  ASSIGN
    {&SELF-NAME}:SCREEN-VALUE = "{&maxRows}"
    {&SELF-NAME}
    .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAfterValueFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAfterValueFilter C-Win
ON RETURN OF svAfterValueFilter IN FRAME AuditSearch /* After Value */
DO:
    APPLY "CHOOSE":U TO btnFilterAfterValue.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svAuditKeyFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAuditKeyFilter C-Win
ON RETURN OF svAuditKeyFilter IN FRAME AuditSearch /* Audit Key */
DO:
    APPLY "CHOOSE":U TO btnFilterAuditKey.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svBeforeValueFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svBeforeValueFilter C-Win
ON RETURN OF svBeforeValueFilter IN FRAME AuditSearch /* Before Value */
DO:
    APPLY "CHOOSE":U TO btnFilterBeforeValue.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svDB C-Win
ON VALUE-CHANGED OF svDB IN FRAME AuditSearch /* DB */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pSetStartEndRange (SELF).
    RUN pGetFilterValues ("TABLE").
    RUN pGetFilterValues ("FIELD").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndDate C-Win
ON HELP OF svEndDate IN FRAME AuditSearch /* To Date */
DO:
    {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndDate C-Win
ON LEAVE OF svEndDate IN FRAME AuditSearch /* To Date */
DO:
    fSetAuditDate ("End").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndDateHour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndDateHour C-Win
ON LEAVE OF svEndDateHour IN FRAME AuditSearch /* Time */
DO:
    IF {&SELF-NAME}:SCREEN-VALUE GT "23" THEN
    ASSIGN
        {&SELF-NAME}:SCREEN-VALUE = "23"
        svEndDateMin:SCREEN-VALUE = "59"
        .
    fSetAuditDate ("End").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndDateMin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndDateMin C-Win
ON LEAVE OF svEndDateMin IN FRAME AuditSearch
DO:
    IF {&SELF-NAME}:SCREEN-VALUE GT "59" THEN
    {&SELF-NAME}:SCREEN-VALUE = "59".
    fSetAuditDate ("End").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndDateOption C-Win
ON VALUE-CHANGED OF svEndDateOption IN FRAME AuditSearch
DO:
    {aoa/includes/tDateOption.i &dateObject=svEndDate &btnCalendar=2}
    APPLY "LEAVE":U TO svEndDate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndRecKeyDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndRecKeyDate C-Win
ON HELP OF svEndRecKeyDate IN FRAME AuditSearch /* To RecKey  Date */
DO:
    {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndRecKeyDate C-Win
ON LEAVE OF svEndRecKeyDate IN FRAME AuditSearch /* To RecKey  Date */
DO:
    fSetAuditRecKey ("End").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndRecKeyDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndRecKeyDateOption C-Win
ON VALUE-CHANGED OF svEndRecKeyDateOption IN FRAME AuditSearch
DO:
    {aoa/includes/tDateOption.i &dateObject=svEndRecKeyDate &btnCalendar=4}
    APPLY "LEAVE":U TO svEndRecKeyDate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndRecKeyHour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndRecKeyHour C-Win
ON LEAVE OF svEndRecKeyHour IN FRAME AuditSearch /* End Time */
DO:
    IF {&SELF-NAME}:SCREEN-VALUE GT "23" THEN
    ASSIGN
        {&SELF-NAME}:SCREEN-VALUE = "23"
        svEndRecKeyMin:SCREEN-VALUE = "59"
        svEndRecKeySec:SCREEN-VALUE = "59"
        .
    fSetAuditRecKey ("End").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndRecKeyMin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndRecKeyMin C-Win
ON LEAVE OF svEndRecKeyMin IN FRAME AuditSearch
DO:
    IF {&SELF-NAME}:SCREEN-VALUE GT "59" THEN
    {&SELF-NAME}:SCREEN-VALUE = "59".
    fSetAuditRecKey ("End").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndRecKeySec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndRecKeySec C-Win
ON LEAVE OF svEndRecKeySec IN FRAME AuditSearch
DO:
    IF {&SELF-NAME}:SCREEN-VALUE GT "59" THEN
    {&SELF-NAME}:SCREEN-VALUE = "59".
    fSetAuditRecKey ("End").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svField
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svField C-Win
ON VALUE-CHANGED OF svField IN FRAME AuditSearch /* Field */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pSetStartEndRange (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartDate C-Win
ON HELP OF svStartDate IN FRAME AuditSearch /* From Date */
DO:
    {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartDate C-Win
ON LEAVE OF svStartDate IN FRAME AuditSearch /* From Date */
DO:
    fSetAuditDate ("Start").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartDateHour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartDateHour C-Win
ON LEAVE OF svStartDateHour IN FRAME AuditSearch /* Time */
DO:
    IF {&SELF-NAME}:SCREEN-VALUE GT "23" THEN
    ASSIGN
        {&SELF-NAME}:SCREEN-VALUE = "23"
        svStartDateMin:SCREEN-VALUE = "59"
        .
    fSetAuditDate ("Start").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartDateMin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartDateMin C-Win
ON LEAVE OF svStartDateMin IN FRAME AuditSearch
DO:
    IF {&SELF-NAME}:SCREEN-VALUE GT "59" THEN
    {&SELF-NAME}:SCREEN-VALUE = "59".
    fSetAuditDate ("Start").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartDateOption C-Win
ON VALUE-CHANGED OF svStartDateOption IN FRAME AuditSearch
DO:
    {AOA/includes/tDateOption.i &dateObject=svStartDate &btnCalendar=1}
    APPLY "LEAVE":U TO svStartDate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartRecKeyDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartRecKeyDate C-Win
ON HELP OF svStartRecKeyDate IN FRAME AuditSearch /* From RecKey Date */
DO:
    {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartRecKeyDate C-Win
ON LEAVE OF svStartRecKeyDate IN FRAME AuditSearch /* From RecKey Date */
DO:
    fSetAuditRecKey ("Start").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartRecKeyDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartRecKeyDateOption C-Win
ON VALUE-CHANGED OF svStartRecKeyDateOption IN FRAME AuditSearch
DO:
    {AOA/includes/tDateOption.i &dateObject=svStartRecKeyDate &btnCalendar=3}
    APPLY "LEAVE":U TO svStartRecKeyDate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartRecKeyHour
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartRecKeyHour C-Win
ON LEAVE OF svStartRecKeyHour IN FRAME AuditSearch /* Start Time */
DO:
    IF {&SELF-NAME}:SCREEN-VALUE GT "23" THEN
    ASSIGN
        {&SELF-NAME}:SCREEN-VALUE = "23"
        svStartRecKeyMin:SCREEN-VALUE = "59"
        svStartRecKeySec:SCREEN-VALUE = "59"
        .
    fSetAuditRecKey ("Start").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartRecKeyMin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartRecKeyMin C-Win
ON LEAVE OF svStartRecKeyMin IN FRAME AuditSearch
DO:
    IF {&SELF-NAME}:SCREEN-VALUE GT "59" THEN
    {&SELF-NAME}:SCREEN-VALUE = "59".
    fSetAuditRecKey ("Start").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartRecKeySec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartRecKeySec C-Win
ON LEAVE OF svStartRecKeySec IN FRAME AuditSearch
DO:
    IF {&SELF-NAME}:SCREEN-VALUE GT "59" THEN
    {&SELF-NAME}:SCREEN-VALUE = "59".
    fSetAuditRecKey ("Start").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svTable C-Win
ON VALUE-CHANGED OF svTable IN FRAME AuditSearch /* Table */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pSetStartEndRange (SELF).
    RUN pGetFilterValues ("FIELD").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svType C-Win
ON VALUE-CHANGED OF svType IN FRAME AuditSearch /* Type */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pSetStartEndRange (SELF).
    RUN pGetFilterValues ("TABLE").
    RUN pGetFilterValues ("FIELD").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svUser
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svUser C-Win
ON VALUE-CHANGED OF svUser IN FRAME AuditSearch /* User ID */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pSetStartEndRange (SELF).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svUseRecKeySearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svUseRecKeySearch C-Win
ON VALUE-CHANGED OF svUseRecKeySearch IN FRAME AuditSearch /* Use Rec Key Search */
DO:
    ASSIGN {&SELF-NAME}.
    IF {&SELF-NAME} THEN DO:
        DISABLE {&searchObjects} WITH FRAME AuditSearch.
        ENABLE {&recKeyFields} {&recKeyObjects} WITH FRAME AuditSearch.
        APPLY "VALUE-CHANGED":U TO svStartRecKeyDateOption.
        APPLY "VALUE-CHANGED":U TO svEndRecKeyDateOption.
    END. /* if */
    ELSE DO:
        HIDE {&recKeyFields} {&recKeyObjects} IN FRAME AuditSearch.
        ENABLE {&searchObjects} WITH FRAME AuditSearch.
        APPLY "VALUE-CHANGED":U TO svStartDateOption.
        APPLY "VALUE-CHANGED":U TO svEndDateOption.
    END. /* else */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME AuditDetail
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
RUN util/CheckModule.p ("ASI","Audit", YES, OUTPUT lContinue).
&ELSE
lContinue = YES.
&ENDIF

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  IF lContinue THEN DO:
    ETIME(YES).
    ASSIGN
        svType:LIST-ITEMS   = "{AOA/includes/auditTypes.i}"
        svType:SCREEN-VALUE = "All"
        svType
        .
    RUN enable_UI.
    IF NOT VALID-HANDLE(hPgmMstrSecur) THEN
    RUN system/PgmMstrSecur.p PERSISTENT SET hPgmMstrSecur.
    IF VALID-HANDLE(hPgmMstrSecur) THEN
    RUN epCanAccess IN hPgmMstrSecur (
        "system/audit.w",
        "Restore",
        OUTPUT lAdmin 
        ).
    fDateOptions (svStartDateOption:HANDLE).
    fDateOptions (svEndDateOption:HANDLE).
    fDateOptions (svStartRecKeyDateOption:HANDLE).
    fDateOptions (svEndRecKeyDateOption:HANDLE).
    RUN pGetFilterValues ("INIT").
    RUN pGetFilterValues ("ALL").
    RUN pGetFilterValues ("TABLE").
    RUN pGetFilterValues ("FIELD").
    &IF DEFINED(AuditHistory) NE 0 &THEN
    /* scop def set in system/CallAudit.p         */
    /* invoked by CTRL-A from viewers or browsers */
    ASSIGN
        lAuditKeyFilter               = YES
        cAuditKeyFilter               = fAuditKey(iphTable,cIdxFlds)
        cStartAuditKey                = cAuditKeyFilter
        cEndAuditKey                  = cAuditKeyFilter
        svAuditKeyFilter:SCREEN-VALUE = cAuditKeyFilter
        svAuditKeyFilter:BGCOLOR      = 14
        {&WINDOW-NAME}:TITLE          = {&WINDOW-NAME}:TITLE
                                      + " [Key: "
                                      + cAuditKeyFilter + "]"
                                      + " Called from "
                                      + ipcType + ": "
                                      + ipcProgramName
                                      .
    &ELSEIF DEFINED(AuditTableList) NE 0 &THEN
    /* scop def set in system/CallAuditList.p    */
    /* invoked by selecting Help > Audit History */
    ASSIGN
        cStartDB             = "ASI"
        cEndDB               = "ASI"
        svDB:SCREEN-VALUE    = "ASI"
        cStartTable          = ipcTable
        cEndTable            = ipcTable
        svTable:SCREEN-VALUE = ipcTable
        {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE
                             + " [Table: "
                             + ipcTable + "]"
                             + " Called from "
                             + ipcType + ": "
                             + ipcProgramName
                             .
    &ELSE
    /* invoked by selecting NS5 from Main Menu */
    RUN pGetSettings.
    &ENDIF
    ASSIGN
        svUseRecKeySearch = NO
        svUseRecKeySearch:SCREEN-VALUE = STRING(svUseRecKeySearch)
        svUseRecKeySearch:SENSITIVE = lAdmin
        .
    RUN pSetStartEndRange (svType:HANDLE).
    RUN pSetStartEndRange (svUser:HANDLE).
    RUN pSetStartEndRange (svDB:HANDLE).
    RUN pSetStartEndRange (svTable:HANDLE).
    RUN pSetStartEndRange (svField:HANDLE).
    RUN pSetStartEndRange (SVAuditKeyFilter:HANDLE).
    RUN pSetStartEndRange (svBeforeValueFilter:HANDLE).
    RUN pSetStartEndRange (svAfterValueFilter:HANDLE).
    APPLY "VALUE-CHANGED":U TO svStartDateOption.
    APPLY "VALUE-CHANGED":U TO svEndDateOption.
    APPLY "VALUE-CHANGED":U TO svStartRecKeyDateOption.
    APPLY "VALUE-CHANGED":U TO svEndRecKeyDateOption.
    APPLY "VALUE-CHANGED":U TO svUseRecKeySearch.
    ASSIGN
        initTime:SCREEN-VALUE = STRING(ETIME / 1000)
        hdAuditHdrQuery = BROWSE AuditHeader:QUERY
        hdAuditDtlQuery = BROWSE AuditDetail:QUERY
        .        
    SESSION:SET-WAIT-STATE("General").
    ETIME(YES).
    &IF DEFINED(AuditHistory) NE 0 OR DEFINED(AuditTableList) NE 0 &THEN
    RUN pPrepareAndExecuteQueryForHeader.
    APPLY "VALUE-CHANGED":U TO BROWSE AuditHeader.
    &ENDIF
    searchTime:SCREEN-VALUE IN FRAME AuditSearch = STRING(ETIME / 1000).
    SESSION:SET-WAIT-STATE("").
  END. /* if lcontinue */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
  IF NOT lContinue THEN
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

&SCOPED-DEFINE FilterFrame AuditSearch
{AOA/includes/pGetAuditQueryFilters.i}

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
  DISPLAY svAuditKeyFilter svType svUser svStartDate svStartDateOption 
          svStartDateHour svStartDateMin svEndDate svEndDateOption svEndDateHour 
          svEndDateMin svDB svTable svField maxRows svBeforeValueFilter 
          svAfterValueFilter svUseRecKeySearch svStartAuditRecKey 
          svEndAuditRecKey svStartRecKeyDate svStartRecKeyDateOption 
          svStartRecKeyHour svStartRecKeyMin svStartRecKeySec svEndRecKeyDate 
          svEndRecKeyDateOption svEndRecKeyHour svEndRecKeyMin svEndRecKeySec 
          searchTime initTime 
      WITH FRAME AuditSearch IN WINDOW C-Win.
  ENABLE btnAfterValueFilterClear btnAuditKeyFilterClear 
         btnBeforeValueFilterClear btnCalendar-1 btnCalendar-2 btnCalendar-3 
         btnCalendar-4 btnFilterAfterValue btnFilterAuditKey 
         btnFilterBeforeValue btnHistory svAuditKeyFilter svType svUser 
         svStartDate svStartDateOption svStartDateHour svStartDateMin svEndDate 
         svEndDateOption svEndDateHour svEndDateMin svDB svTable svField 
         maxRows btnPrint svBeforeValueFilter svAfterValueFilter 
         svUseRecKeySearch svStartAuditRecKey svEndAuditRecKey 
         svStartRecKeyDate svStartRecKeyDateOption svStartRecKeyHour 
         svStartRecKeyMin svStartRecKeySec svEndRecKeyDate 
         svEndRecKeyDateOption svEndRecKeyHour svEndRecKeyMin svEndRecKeySec 
         btnStack btnAuditTables btnClear btnSearch 
      WITH FRAME AuditSearch IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-AuditSearch}
  DISPLAY svSortByHdr svSortByDtl 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE AuditHeader AuditDetail svSortByHdr svSortByDtl 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  IF AVAILABLE AuditDtl THEN 
    DISPLAY AuditDtl.AuditField AuditDtl.AuditIdxField AuditDtl.AuditBeforeValue 
          AuditDtl.AuditExtent AuditDtl.AuditAfterValue 
      WITH FRAME AuditView IN WINDOW C-Win.
  IF AVAILABLE AuditHdr THEN 
    DISPLAY AuditHdr.AuditDB AuditHdr.AuditTable AuditHdr.AuditType 
          AuditHdr.AuditID AuditHdr.AuditUser AuditHdr.AuditDateTime 
      WITH FRAME AuditView IN WINDOW C-Win.
  VIEW FRAME AuditView IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-AuditView}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAuditKeyFilter C-Win 
PROCEDURE pAuditKeyFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    OPEN QUERY AuditHeader
        FOR EACH AuditHdr NO-LOCK
            WHERE AuditHdr.AuditDB       GE cStartDB
              AND AuditHdr.AuditDB       LE cEndDB
              AND AuditHdr.AuditTable    GE cStartTable
              AND AuditHdr.AuditTable    LE cEndTable
              AND AuditHdr.AuditDateTime GE dtStartDateTime
              AND AuditHdr.AuditDateTime LE dtEndDateTime
              AND AuditHdr.AuditType     GE cStartType
              AND AuditHdr.AuditType     LE cEndType
              AND AuditHdr.AuditUser     GE cStartUser
              AND AuditHdr.AuditUser     LE cEndUser
              AND AuditHdr.AuditKey      GE cStartAuditKey
              AND AuditHdr.AuditKey      LE cEndAuditKey,
            FIRST AuditDtl OF AuditHdr NO-LOCK
            WHERE AuditDtl.AuditField       GE cStartField
              AND AuditDtl.AuditField       LE cEndField
              AND AuditDtl.AuditBeforeValue GE cStartBeforeValue
              AND AuditDtl.AuditBeforeValue LE cEndBeforeValue
              AND AuditDtl.AuditAfterValue  GE cStartAfterValue
              AND AuditDtl.AuditAfterValue  LE cEndAfterValue
            INDEXED-REPOSITION
            {&MAX-ROWS}
            .
    APPLY "VALUE-CHANGED":U TO BROWSE AuditHeader.
    
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
    DEFINE VARIABLE hChild AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx    AS INTEGER NO-UNDO.
    
    FIND FIRST user-print NO-LOCK
         WHERE user-print.company    EQ g_company
           AND user-print.program-id EQ "Audit."
           AND user-print.user-id    EQ USERID("ASI")
         NO-ERROR.
    IF NOT AVAILABLE user-print THEN RETURN.
    ASSIGN
        hChild = FRAME AuditSearch:FIRST-CHILD
        hChild = hChild:FIRST-CHILD
        .
    DO idx = 1 TO EXTENT(user-print.field-name):
        IF user-print.field-name[idx] EQ "" THEN
        CASE user-print.field-label[idx]:
            WHEN "Column" THEN
            {&WINDOW-NAME}:COLUMN = INTEGER(user-print.field-value[idx]).
            WHEN "Row" THEN
            {&WINDOW-NAME}:ROW    = INTEGER(user-print.field-value[idx]).
            WHEN "Width" THEN
            {&WINDOW-NAME}:WIDTH  = INTEGER(user-print.field-value[idx]).
            WHEN "Height" THEN
            {&WINDOW-NAME}:HEIGHT = INTEGER(user-print.field-value[idx]).
        END CASE.
    END. /* do idx */
    RUN pWinReSize.
    ASSIGN {&userPrintFields}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pHistory C-Win 
PROCEDURE pHistory :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcAuditKey AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lShowDetail AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lShowDelete AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lSaveFilter AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER AuditHdr FOR AuditHdr.
    
    MESSAGE
        "Show Field Detail?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
            UPDATE lShowDetail.
    IF lShowDetail EQ ? THEN RETURN.
    
    IF lShowDetail EQ YES THEN
    MESSAGE
        "Show DELETE Detail?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
            UPDATE lShowDelete.
    IF lShowDelete EQ ? THEN RETURN.
    
    ASSIGN
        lSaveFilter     = lAuditKeyFilter
        lAuditKeyFilter = YES
        cAuditKeyFilter = ipcAuditKey
        cStartAuditKey  = ipcAuditKey
        cEndAuditKey    = ipcAuditKey
        .
    OUTPUT TO c:\tmp\AuditHistory.txt.
    PUT UNFORMATTED "Audit History for: " cAuditKeyFilter SKIP.
    &SCOPED-DEFINE SORTBY-PHRASE BY AuditHdr.AuditDateTime DESCENDING
    {&QUERY-STRING-AuditHeader}
        WITH STREAM-IO WIDTH 200:
        DISPLAY
            AuditHdr.AuditDateTime
            AuditHdr.AuditType
            AuditHdr.AuditUser
            AuditHdr.AuditDB
            AuditHdr.AuditTable
            .
        IF lShowDetail THEN
        RUN pHistoryDetail (BUFFER AuditHdr, lShowDelete).
    END. /* query-string */
    OUTPUT CLOSE.
    ASSIGN
        lAuditKeyFilter = lSaveFilter
        cAuditKeyFilter = IF lAuditKeyFilter THEN ipcAuditKey ELSE ""
        cStartAuditKey  = IF lAuditKeyFilter THEN ipcAuditKey ELSE CHR(32)
        cEndAuditKey    = IF lAuditKeyFilter THEN ipcAuditKey ELSE CHR(254)
        .
&IF DEFINED(FWD-VERSION) > 0 &THEN
    open-mime-resource "text/plain" "file:///c:\tmp\AuditHistory.txt" FALSE.
&ELSE
    OS-COMMAND NO-WAIT notepad.exe c:\tmp\AuditHistory.txt.
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pHistoryDetail C-Win 
PROCEDURE pHistoryDetail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER AuditHdr FOR AuditHdr.
    DEFINE INPUT PARAMETER iplShowDelete AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cField AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER AuditDtl FOR AuditDtl.

    {&QUERY-STRING-AuditDetail}
        WITH STREAM-IO WIDTH 200:
        IF AuditDtl.AuditIdxField AND
           AuditDtl.AuditBeforeValue EQ
           AuditDtl.AuditAfterValue THEN NEXT.
        IF AuditHdr.AuditType EQ "DELETE" AND iplShowDelete EQ NO THEN NEXT.
        cField = AuditDtl.AuditField
               + IF AuditDtl.AuditExtent NE 0 THEN "["
               + STRING(AuditDtl.AuditExtent) + "]" ELSE "".
        DISPLAY
            AuditDtl.AuditField AT 10
            cField @ AuditDtl.AuditField
            AuditDtl.AuditBeforeValue COLUMN-LABEL "Before / After" FORMAT "x(80)"
            .
        IF AuditHdr.AuditType EQ "CREATE" THEN
        DISPLAY AuditDtl.AuditAfterValue @ AuditDtl.AuditBeforeValue.
        ELSE
        IF AuditHdr.AuditType EQ "UPDATE" THEN DO:
            DOWN.
            DISPLAY AuditDtl.AuditAfterValue @ AuditDtl.AuditBeforeValue.
        END. /* if update */
    END. /* each auditdtl */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrepareAndExecuteQueryForDetail C-Win 
PROCEDURE pPrepareAndExecuteQueryForDetail PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cQuery  AS CHARACTER NO-UNDO.
    
    cQuery = "FOR EACH AuditDtl NO-LOCK "
           + "WHERE AuditDtl.AuditID EQ " + STRING (AuditHdr.AuditID) 
           + ( IF cStartField        EQ CHR(32) AND cEndField       EQ CHR(254) THEN "" ELSE " AND AuditDtl.AuditField EQ '"       + cStartField + "'")
           + ( IF cStartBeforeValue  EQ CHR(32) AND cEndBeforeValue EQ CHR(254) THEN "" ELSE " AND AuditDtl.AuditBeforeValue EQ '" + cStartBeforeValue + "'")
           + ( IF cStartAfterValue   EQ CHR(32) AND cEndAfterValue  EQ CHR(254) THEN "" ELSE " AND AuditDtl.AuditAfterValue EQ '"  + cStartAfterValue + "'")
           + ( IF cSortBy NE "" AND NOT lHeaderSorting THEN " BY " + cSortBy + ( IF lAscending THEN "" ELSE " DESCENDING")  ELSE " ")
           .
    SESSION:SET-WAIT-STATE("General").
    hdAuditDtlQuery:QUERY-PREPARE (cQuery).    
    hdAuditDtlQuery:QUERY-OPEN().
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrepareAndExecuteQueryForHeader C-Win 
PROCEDURE pPrepareAndExecuteQueryForHeader PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cQuery  AS CHARACTER NO-UNDO.
    
    cQuery = "FOR EACH AuditHdr NO-LOCK "
           + "WHERE AuditHdr.AuditDateTime GE DATETIME('" + STRING(dtStartDateTime) + "')"
           +   " AND AuditHdr.AuditDateTime LE DATETIME('" + STRING(dtEndDateTime) + "')"
           + ( IF cStartDB       EQ CHR(32) AND cEndDB       EQ CHR(254) THEN "" ELSE " AND AuditHdr.AuditDB    EQ '" + cStartDb + "'")  
           + ( IF cStartTable    EQ CHR(32) AND cEndTable    EQ CHR(254) THEN "" ELSE " AND AuditHdr.AuditTable EQ '" + cStartTable + "'")
           + ( IF cStartType     EQ CHR(32) AND cEndType     EQ CHR(254) THEN "" ELSE " AND AuditHdr.AuditType  EQ '" + cStartType + "'")
           + ( IF cStartUser     EQ CHR(32) AND cEndUser     EQ CHR(254) THEN "" ELSE " AND AuditHdr.AuditUser  EQ '" + cStartUser + "'")
           + ( IF cStartAuditKey EQ CHR(32) AND cEndAuditKey EQ CHR(254) THEN "" ELSE " AND AuditHdr.Auditkey   EQ '" + cStartAuditKey + "'")
           + ", FIRST AuditDtl OF AuditHdr NO-LOCK WHERE TRUE"
           + ( IF cStartField        EQ CHR(32) AND cEndField       EQ CHR(254) THEN "" ELSE " AND AuditDtl.AuditField       EQ '" + cStartField + "'")
           + ( IF cStartBeforeValue  EQ CHR(32) AND cEndBeforeValue EQ CHR(254) THEN "" ELSE " AND AuditDtl.AuditBeforeValue EQ '" + cStartBeforeValue + "'")
           + ( IF cStartAfterValue   EQ CHR(32) AND cEndAfterValue  EQ CHR(254) THEN "" ELSE " AND AuditDtl.AuditAfterValue  EQ '" + cStartAfterValue + "'")
           + ( IF cSortBy NE "" THEN "BY " + cSortBy +( IF lAscending THEN "" ELSE " DESCENDING")  ELSE " ")
           +  " MAX-ROWS " + STRING(maxrows)
           . 
    SESSION:SET-WAIT-STATE("General").
    hdAuditHdrQuery:QUERY-PREPARE(cQuery).    
    hdAuditHdrQuery:QUERY-OPEN().
    SESSION:SET-WAIT-STATE("").

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
    ETIME(YES).
    IF svUseRecKeySearch EQ NO THEN DO:
        CASE cColumnLabel:
            WHEN "AuditType" THEN
                cSortBy = "AuditHdr.AuditType".
            WHEN "AuditDateTime" THEN
                cSortBy = "AuditHdr.AuditDateTime".
            WHEN "AuditDB" THEN
                cSortBy = "AuditHdr.AuditDB".
            WHEN "AuditTable" THEN
                cSortBy = "AuditHdr.AuditTable".
            WHEN "AuditUser" THEN
                cSortBy = "AuditHdr.AuditUser".
            WHEN "AuditIdxField" THEN
                cSortBy = "AuditDtl.AuditIdxField".
            WHEN "AuditField" THEN
                cSortBy = "AuditDtl.AuditField BY AuditDtl.AuditExtent".
            WHEN "AuditKey" THEN
                cSortBy = "AuditHdr.AuditKey".
            WHEN "AuditRecKey" THEN
                cSortBy = "AuditHdr.AuditRecKey".
            WHEN "AuditExtent" THEN
                cSortBy = "AuditDtl.AuditExtent".
            WHEN "AuditBeforeValue" THEN
                cSortBy = "AuditDtl.AuditBeforeValue".
            WHEN "AuditAfterValue" THEN
                cSortBy = "AuditDtl.AuditAfterValue".
            WHEN "AuditID" THEN
                cSortBy = "AuditHdr.AuditId".
            OTHERWISE
                cSortBy = "".
        END CASE.
        IF cCurrentBrowse EQ "AuditHeader" THEN 
            RUN pPrepareAndExecuteQueryForHeader.
        ELSE 
            RUN pPrepareAndExecuteQueryForDetail. 
    END.
    ELSE DO:
        OPEN QUERY AuditHeader
        FOR EACH AuditHdr NO-LOCK
            WHERE AuditHdr.AuditDB       GE cStartDB
              AND AuditHdr.AuditDB       LE cEndDB
              AND AuditHdr.AuditTable    GE cStartTable
              AND AuditHdr.AuditTable    LE cEndTable
              AND AuditHdr.AuditDateTime GE dtStartDateTime
              AND AuditHdr.AuditDateTime LE dtEndDateTime
              AND AuditHdr.AuditRecKey   GE svStartAuditRecKey
              AND AuditHdr.AuditRecKey   LE svEndAuditRecKey,
            FIRST AuditDtl OF AuditHdr NO-LOCK
            INDEXED-REPOSITION
            {&MAX-ROWS}.
    END. /* else */
    SESSION:SET-WAIT-STATE("").
    IF lHeaderSorting THEN
    APPLY "VALUE-CHANGED":U TO BROWSE AuditHeader.
    searchTime:SCREEN-VALUE IN FRAME AuditSearch = STRING(ETIME / 1000).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRestore C-Win 
PROCEDURE pRestore :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hTable AS HANDLE NO-UNDO.
    
    DEFINE BUFFER bAuditDtl FOR AuditDtl.
    
    CREATE BUFFER hTable FOR TABLE AuditHdr.AuditTable.
    
    hTable:BUFFER-CREATE().
    FOR EACH bAuditDtl OF AuditHdr NO-LOCK:
        hTable:BUFFER-FIELD(bAuditDtl.AuditField):BUFFER-VALUE(bAuditDtl.AuditExtent) = bAuditDtl.AuditBeforeValue.
    END. /* each auditdtl */
    FIND CURRENT AuditHdr EXCLUSIVE-LOCK.
    AuditHdr.AuditType = "RESTORED".
    FIND CURRENT AuditHdr NO-LOCK.
    BROWSE AuditHeader:REFRESH().
    MESSAGE
        "Deleted Record Restored"
    VIEW-AS ALERT-BOX INFORMATION.
    
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
    DEFINE VARIABLE hChild AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx    AS INTEGER NO-UNDO.
    
    FIND FIRST user-print EXCLUSIVE-LOCK
         WHERE user-print.company    EQ g_company
           AND user-print.program-id EQ "Audit."
           AND user-print.user-id    EQ USERID("ASI")
         NO-ERROR.
    IF NOT AVAILABLE user-print THEN DO:
        CREATE user-print.
        ASSIGN
            user-print.company    = g_company
            user-print.program-id = "Audit."
            user-print.user-id    = USERID("ASI")
            .
    END. /* not avail */
    ASSIGN
        user-print.field-name  = ""
        user-print.field-value = ""
        user-print.field-label = ""
        hChild = FRAME AuditSearch:FIRST-CHILD
        hChild = hChild:FIRST-CHILD
        .
    DO WHILE VALID-HANDLE(hChild):
        IF hChild:NAME NE ? AND
          (hChild:SENSITIVE OR
           hChild:TYPE EQ "COMBO-BOX") AND
           hChild:TYPE NE "Button" THEN
        ASSIGN
            idx = idx + 1
            user-print.field-name[idx]  = hChild:NAME
            user-print.field-label[idx] = hChild:LABEL
            user-print.field-value[idx] = hChild:SCREEN-VALUE
            .
        hChild = hChild:NEXT-SIBLING.
    END. /* do while */
    ASSIGN
        idx = idx + 1
        user-print.field-name[idx]  = ""
        user-print.field-label[idx] = "Column"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:COLUMN)
        idx = idx + 1
        user-print.field-name[idx]  = ""
        user-print.field-label[idx] = "Row"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:ROW)
        idx = idx + 1
        user-print.field-name[idx]  = ""
        user-print.field-label[idx] = "Width"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:WIDTH)
        idx = idx + 1
        user-print.field-name[idx]  = ""
        user-print.field-label[idx] = "Height"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:HEIGHT)
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetStartEndRange C-Win 
PROCEDURE pSetStartEndRange :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphSelf AS HANDLE NO-UNDO.
    
    CASE iphSelf:NAME:
        WHEN "svDB" THEN
        ASSIGN
            cStartDB = IF iphSelf:SCREEN-VALUE NE "All" THEN iphSelf:SCREEN-VALUE
                       ELSE CHR(32)
            cEndDB   = IF iphSelf:SCREEN-VALUE NE "All" THEN iphSelf:SCREEN-VALUE
                       ELSE CHR(254)
            .
        WHEN "svField" THEN
        ASSIGN
            cStartField = IF iphSelf:SCREEN-VALUE NE "All" THEN iphSelf:SCREEN-VALUE
                          ELSE CHR(32)
            cEndField   = IF iphSelf:SCREEN-VALUE NE "All" THEN iphSelf:SCREEN-VALUE
                          ELSE CHR(254)
            .
        WHEN "svTable" THEN
        ASSIGN
            cStartTable = IF iphSelf:SCREEN-VALUE NE "All" THEN iphSelf:SCREEN-VALUE
                          ELSE CHR(32)
            cEndTable   = IF iphSelf:SCREEN-VALUE NE "All" THEN iphSelf:SCREEN-VALUE
                          ELSE CHR(254)
            .
        WHEN "svType" THEN
        ASSIGN
            cStartType = IF iphSelf:SCREEN-VALUE NE "All" THEN iphSelf:SCREEN-VALUE
                         ELSE CHR(32)
            cEndType   = IF iphSelf:SCREEN-VALUE NE "All" THEN iphSelf:SCREEN-VALUE
                         ELSE CHR(254)
            .
        WHEN "svUser" THEN
        ASSIGN
            cStartUser = IF iphSelf:SCREEN-VALUE NE "All" THEN iphSelf:SCREEN-VALUE
                         ELSE CHR(32)
            cEndUser   = IF iphSelf:SCREEN-VALUE NE "All" THEN iphSelf:SCREEN-VALUE
                         ELSE CHR(254)
            .
        WHEN "svAuditKeyFilter" THEN
        ASSIGN
            cStartAuditKey = IF lAuditKeyFilter THEN iphSelf:SCREEN-VALUE
                             ELSE CHR(32)
            cEndAuditKey   = IF lAuditKeyFilter THEN iphSelf:SCREEN-VALUE
                             ELSE CHR(254)
            .
        WHEN "svBeforeValueFilter" THEN
        ASSIGN
            cStartBeforeValue = IF lBeforeValueFilter THEN iphSelf:SCREEN-VALUE
                                ELSE CHR(32)
            cEndBeforeValue   = IF lBeforeValueFilter THEN iphSelf:SCREEN-VALUE
                                ELSE CHR(254)
            .
        WHEN "svAfterValueFilter" THEN
        ASSIGN
            cStartAfterValue = IF lAfterValueFilter THEN iphSelf:SCREEN-VALUE
                               ELSE CHR(32)
            cEndAfterValue   = IF lAfterValueFilter THEN iphSelf:SCREEN-VALUE
                               ELSE CHR(254)
            .
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUserPrint C-Win 
PROCEDURE pUserPrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hWidget AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    
    FIND FIRST user-print EXCLUSIVE-LOCK
         WHERE user-print.company    EQ g_company
           AND user-print.program-id EQ "AuditHist."
           AND user-print.user-id    EQ USERID("ASI")
         NO-ERROR.
    IF NOT AVAILABLE user-print THEN DO:
        CREATE user-print.
        ASSIGN
            user-print.company    = g_company
            user-print.program-id = "AuditHist."
            user-print.user-id    = USERID("ASI")
            .
    END. /* not avail */
    ASSIGN
        user-print.field-name[1]  = "Company"
        user-print.field-label[1] = "Company"
        user-print.field-value[1] = g_company
        hWidget = FRAME AuditSearch:FIRST-CHILD
        hWidget = hWidget:FIRST-CHILD
        .
    DO WHILE VALID-HANDLE(hWidget):
        IF hWidget:PRIVATE-DATA EQ "parameter" THEN
        ASSIGN
            idx = idx + 1
            user-print.field-name[idx]  = hWidget:NAME
            user-print.field-label[idx] = hWidget:LABEL
            user-print.field-value[idx] = hWidget:SCREEN-VALUE
            .
        hWidget = hWidget:NEXT-SIBLING.
    END. /* do while */
    FIND CURRENT user-print NO-LOCK.

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
    DEFINE VARIABLE iHeight AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iWidth  AS DECIMAL NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:
        HIDE FRAME {&FRAME-NAME}.
        HIDE BROWSE AuditHeader BROWSE AuditDetail.
        HIDE FRAME AuditView.
        IF {&WINDOW-NAME}:HEIGHT LT 28.57 THEN
        {&WINDOW-NAME}:HEIGHT = 28.57.
        IF {&WINDOW-NAME}:WIDTH  LT 238.2 THEN
        {&WINDOW-NAME}:WIDTH  = 238.2.
        ASSIGN
            iHeight = {&WINDOW-NAME}:HEIGHT - FRAME {&FRAME-NAME}:HEIGHT
            iWidth  = {&WINDOW-NAME}:WIDTH  - FRAME {&FRAME-NAME}:WIDTH
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH
            FRAME {&FRAME-NAME}:HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:WIDTH  = {&WINDOW-NAME}:WIDTH
            BROWSE AuditHeader:HEIGHT = BROWSE AuditHeader:HEIGHT + iHeight
            BROWSE AuditDetail:HEIGHT = BROWSE AuditDetail:HEIGHT + iHeight
            FRAME AuditView:ROW = FRAME AuditView:ROW + iHeight
            .
        VIEW FRAME {&FRAME-NAME}.
        VIEW BROWSE AuditHeader BROWSE AuditDetail.
        VIEW FRAME AuditView.
        svSortByHdr:MOVE-TO-TOP().
        svSortByDtl:MOVE-TO-TOP().
    END. /* do with */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fRecKeyDate C-Win 
FUNCTION fRecKeyDate RETURNS DATE
  ( ipcAuditRecKey AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iDay   AS INTEGER NO-UNDO INITIAL ?.
    DEFINE VARIABLE iMonth AS INTEGER NO-UNDO INITIAL ?.
    DEFINE VARIABLE iYear  AS INTEGER NO-UNDO INITIAL ?.

    IF LENGTH(ipcAuditRecKey) EQ 21 THEN
    ASSIGN
        iDay   = INTEGER(SUBSTRING(ipcAuditRecKey,5,2))
        iMonth = INTEGER(SUBSTRING(ipcAuditRecKey,7,2))
        iYear  = INTEGER(SUBSTRING(ipcAuditRecKey,1,4))
        .
    ELSE
    IF ipcAuditRecKey NE "" THEN
    ASSIGN
        iDay   = INTEGER(SUBSTRING(ipcAuditRecKey,1,2))
        iMonth = INTEGER(SUBSTRING(ipcAuditRecKey,3,2))
        iYear  = INTEGER(SUBSTRING(ipcAuditRecKey,5,4))
        .
    RETURN DATE(iDay,iMonth,iYear).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fRecKeyTime C-Win 
FUNCTION fRecKeyTime RETURNS CHARACTER
  ( ipcAuditRecKey AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iTime AS INTEGER NO-UNDO.

    IF LENGTH(ipcAuditRecKey) EQ 21 THEN
    iTime = INTEGER(SUBSTRING(ipcAuditRecKey,9,5)).
    
    RETURN STRING(iTime,"HH:MM:SS").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fSetAuditDate C-Win 
FUNCTION fSetAuditDate RETURNS CHARACTER
  ( ipcType AS CHARACTER ) :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME AuditSearch:
        ASSIGN {&auditDateFields}.
        CASE ipcType:
            WHEN "Start" THEN
            dtStartDateTime = DATETIME(svStartDate, (svStartDateHour * 3600 + svStartDateMin * 60) * 1000).
            WHEN "End" THEN
            dtEndDateTime = DATETIME(svEndDate, (svEndDateHour * 3600 + svEndDateMin * 60 + 59) * 1000 + 999).
        END CASE.
    END.
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fSetAuditRecKey C-Win 
FUNCTION fSetAuditRecKey RETURNS CHARACTER
  ( ipcType AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DO WITH FRAME AuditSearch:
        ASSIGN {&recKeyFields}.
        CASE ipcType:
            WHEN "Start" THEN
            ASSIGN
                svStartAuditRecKey = STRING(YEAR(svStartRecKeyDate),"9999")
                                   + STRING(MONTH(svStartRecKeyDate),"99")
                                   + STRING(DAY(svStartRecKeyDate),"99")
                                   + STRING(svStartRecKeyHour * 3600
                                   + svStartRecKeyMin * 60
                                   + svStartRecKeySec,"99999")
                                   + "00000000"
                svStartAuditRecKey:SCREEN-VALUE = svStartAuditRecKey
                .
            WHEN "End" THEN
            ASSIGN
                svEndAuditRecKey = STRING(YEAR(svEndRecKeyDate),"9999")
                                 + STRING(MONTH(svEndRecKeyDate),"99")
                                 + STRING(DAY(svEndRecKeyDate),"99")
                                 + STRING(svEndRecKeyHour * 3600
                                 + svEndRecKeyMin * 60
                                 + svEndRecKeySec,"99999")
                                 + "99999999"
                svEndAuditRecKey:SCREEN-VALUE = svEndAuditRecKey
                .
        END CASE.
    END.
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

