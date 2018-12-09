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
&Scoped-define maxRows 2500

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{methods/defines/sortByDefs.i}

DEFINE VARIABLE lContinue          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lHeaderSorting     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE dtStartDateTime    AS DATETIME  NO-UNDO.
DEFINE VARIABLE dtEndDateTime      AS DATETIME  NO-UNDO.
DEFINE VARIABLE cAuditKeyFilter    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAuditKeyFilter    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lBeforeValueFilter AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cBeforeValueFilter AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAfterValueFilter  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cAfterValueFilter  AS CHARACTER NO-UNDO.
DEFINE VARIABLE hPgmMstrSecur      AS HANDLE    NO-UNDO.
DEFINE VARIABLE lAdmin             AS LOGICAL   NO-UNDO.

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
AND (AuditDtl.AuditField EQ svField ~
OR svField EQ "ALL") ~
AND (AuditDtl.AuditBeforeValue BEGINS cBeforeValueFilter ~
OR cBeforeValueFilter EQ "") ~
AND (AuditDtl.AuditAfterValue BEGINS cAfterValueFilter ~
OR cAfterValueFilter EQ "") NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-AuditDetail OPEN QUERY AuditDetail FOR EACH AuditDtl ~
      WHERE AuditDtl.AuditID EQ AuditHdr.AuditID ~
AND (AuditDtl.AuditField EQ svField ~
OR svField EQ "ALL") ~
AND (AuditDtl.AuditBeforeValue BEGINS cBeforeValueFilter ~
OR cBeforeValueFilter EQ "") ~
AND (AuditDtl.AuditAfterValue BEGINS cAfterValueFilter ~
OR cAfterValueFilter EQ "") NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-AuditDetail AuditDtl
&Scoped-define FIRST-TABLE-IN-QUERY-AuditDetail AuditDtl


/* Definitions for BROWSE AuditHeader                                   */
&Scoped-define FIELDS-IN-QUERY-AuditHeader AuditHdr.AuditID ~
AuditHdr.AuditType AuditHdr.AuditDateTime AuditHdr.AuditDB ~
AuditHdr.AuditTable AuditHdr.AuditUser AuditHdr.AuditKey ~
AuditHdr.AuditStackID 
&Scoped-define ENABLED-FIELDS-IN-QUERY-AuditHeader 
&Scoped-define QUERY-STRING-AuditHeader FOR EACH AuditHdr ~
      WHERE AuditHdr.AuditDateTime GE dtStartDateTime ~
AND AuditHdr.AuditDateTime LE dtEndDateTime ~
AND (AuditHdr.AuditType EQ svType OR svType EQ "ALL") ~
AND (AuditHdr.AuditUser EQ svUser OR svUser EQ "ALL") ~
AND (AuditHdr.AuditDB EQ svDB OR svDB EQ "ALL") ~
AND (AuditHdr.AuditTable EQ svTable OR svTable EQ "ALL") ~
AND (AuditHdr.AuditKey EQ cAuditKeyFilter ~
OR lAuditKeyFilter EQ FALSE) ~
 NO-LOCK, ~
      FIRST AuditDtl OF AuditHdr ~
      WHERE (AuditDtl.AuditField EQ svField ~
OR svField EQ "ALL") ~
AND (AuditDtl.AuditBeforeValue BEGINS cBeforeValueFilter ~
OR cBeforeValueFilter EQ "") ~
AND (AuditDtl.AuditAfterValue BEGINS cAfterValueFilter ~
OR cAfterValueFilter EQ "") NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-AuditHeader OPEN QUERY AuditHeader FOR EACH AuditHdr ~
      WHERE AuditHdr.AuditDateTime GE dtStartDateTime ~
AND AuditHdr.AuditDateTime LE dtEndDateTime ~
AND (AuditHdr.AuditType EQ svType OR svType EQ "ALL") ~
AND (AuditHdr.AuditUser EQ svUser OR svUser EQ "ALL") ~
AND (AuditHdr.AuditDB EQ svDB OR svDB EQ "ALL") ~
AND (AuditHdr.AuditTable EQ svTable OR svTable EQ "ALL") ~
AND (AuditHdr.AuditKey EQ cAuditKeyFilter ~
OR lAuditKeyFilter EQ FALSE) ~
 NO-LOCK, ~
      FIRST AuditDtl OF AuditHdr ~
      WHERE (AuditDtl.AuditField EQ svField ~
OR svField EQ "ALL") ~
AND (AuditDtl.AuditBeforeValue BEGINS cBeforeValueFilter ~
OR cBeforeValueFilter EQ "") ~
AND (AuditDtl.AuditAfterValue BEGINS cAfterValueFilter ~
OR cAfterValueFilter EQ "") NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-AuditHeader AuditHdr AuditDtl
&Scoped-define FIRST-TABLE-IN-QUERY-AuditHeader AuditHdr
&Scoped-define SECOND-TABLE-IN-QUERY-AuditHeader AuditDtl


/* Definitions for FRAME AuditSearch                                    */
&Scoped-define FIELDS-IN-QUERY-AuditSearch AuditHdr.AuditKey 
&Scoped-define QUERY-STRING-AuditSearch FOR EACH AuditHdr SHARE-LOCK
&Scoped-define OPEN-QUERY-AuditSearch OPEN QUERY AuditSearch FOR EACH AuditHdr SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-AuditSearch AuditHdr
&Scoped-define FIRST-TABLE-IN-QUERY-AuditSearch AuditHdr


/* Definitions for FRAME AuditView                                      */
&Scoped-define FIELDS-IN-QUERY-AuditView AuditHdr.AuditDB ~
AuditHdr.AuditType AuditHdr.AuditID AuditHdr.AuditUser ~
AuditHdr.AuditDateTime AuditHdr.AuditTable AuditDtl.AuditIdxField ~
AuditDtl.AuditBeforeValue AuditDtl.AuditField AuditDtl.AuditExtent ~
AuditDtl.AuditAfterValue 
&Scoped-define QUERY-STRING-AuditView FOR EACH AuditDtl SHARE-LOCK, ~
      EACH AuditHdr OF AuditDtl SHARE-LOCK
&Scoped-define OPEN-QUERY-AuditView OPEN QUERY AuditView FOR EACH AuditDtl SHARE-LOCK, ~
      EACH AuditHdr OF AuditDtl SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-AuditView AuditDtl AuditHdr
&Scoped-define FIRST-TABLE-IN-QUERY-AuditView AuditDtl
&Scoped-define SECOND-TABLE-IN-QUERY-AuditView AuditHdr


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-AuditDetail}~
    ~{&OPEN-QUERY-AuditHeader}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS AuditHeader AuditDetail btnStack btnPrint ~
svSortByHdr svSortByDtl 
&Scoped-Define DISPLAYED-OBJECTS svSortByHdr svSortByDtl 

/* Custom List Definitions                                              */
/* userPrintFields,List-2,List-3,List-4,List-5,List-6                   */
&Scoped-define userPrintFields svType svStartDate svStartDateOption svDB ~
maxRows svUser svEndDate svEndDateOption svTable svField 
&Scoped-define List-3 btnCalendar-1 btnCalendar-2 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAfterValueFilterClear 
     IMAGE-UP FILE "Graphics/16x16/navigate_cross.png":U NO-FOCUS
     LABEL "" 
     SIZE 4.4 BY 1.05 TOOLTIP "Clear After Value Filter".

DEFINE BUTTON btnAuditTables 
     IMAGE-UP FILE "Graphics/32x32/checks.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8.4 BY 2 TOOLTIP "Select Tables to Audit"
     FONT 4.

DEFINE BUTTON btnBeforeValueFilterClear 
     IMAGE-UP FILE "Graphics/16x16/navigate_cross.png":U NO-FOCUS
     LABEL "" 
     SIZE 4.4 BY 1.05 TOOLTIP "Clear Before Value Filter".

DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-2 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnClear 
     IMAGE-UP FILE "Graphics/32x32/erase.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Clear" 
     SIZE 8.4 BY 2 TOOLTIP "Clear Filter Values".

DEFINE BUTTON btnFilterAfterValue 
     IMAGE-UP FILE "Graphics/16x16/filterwindow.bmp":U NO-FOCUS
     LABEL "Filter" 
     SIZE 4 BY .95 TOOLTIP "Filter By After Value".

DEFINE BUTTON btnFilterAuditKey 
     IMAGE-UP FILE "Graphics/16x16/filterwindow.bmp":U NO-FOCUS
     LABEL "Filter" 
     SIZE 4 BY .95 TOOLTIP "Filter By Audit Key".

DEFINE BUTTON btnFilterBeforeValue 
     IMAGE-UP FILE "Graphics/16x16/filterwindow.bmp":U NO-FOCUS
     LABEL "Filter" 
     SIZE 4 BY .95 TOOLTIP "Filter By Before Value".

DEFINE BUTTON btnHistory 
     IMAGE-UP FILE "Graphics/16x16/print.bmp":U NO-FOCUS
     LABEL "History" 
     SIZE 4 BY .95 TOOLTIP "View History".

DEFINE BUTTON btnSearch 
     IMAGE-UP FILE "Graphics/32x32/magnifying_glass.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Search" 
     SIZE 8.4 BY 2 TOOLTIP "Search".

DEFINE VARIABLE svDB AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
     LABEL "DB" 
     VIEW-AS COMBO-BOX INNER-LINES 4
     LIST-ITEMS "All" 
     DROP-DOWN-LIST
     SIZE 26 BY 1 TOOLTIP "Select Audit DB Filter" NO-UNDO.

DEFINE VARIABLE svEndDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svField AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
     LABEL "Field" 
     VIEW-AS COMBO-BOX INNER-LINES 4
     LIST-ITEMS "All" 
     DROP-DOWN-LIST
     SIZE 26 BY 1 TOOLTIP "Select Audit Field Filter" NO-UNDO.

DEFINE VARIABLE svStartDateOption AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE svTable AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
     LABEL "Table" 
     VIEW-AS COMBO-BOX INNER-LINES 4
     LIST-ITEMS "All" 
     DROP-DOWN-LIST
     SIZE 26 BY 1 TOOLTIP "Select Audit Table Filter" NO-UNDO.

DEFINE VARIABLE svType AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
     LABEL "Type" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "All","CREATE","DELETE","UPDATE","RESTORED","TRACK","LOG" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 TOOLTIP "Select Audit Type Filter" NO-UNDO.

DEFINE VARIABLE svUser AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
     LABEL "User ID" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "All" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 TOOLTIP "Select User Filter" NO-UNDO.

DEFINE VARIABLE maxRows AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 2500 
     LABEL "Max Rows" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE svAfterValueFilter AS CHARACTER FORMAT "X(256)":U 
     LABEL "After Value" 
     VIEW-AS FILL-IN 
     SIZE 70.4 BY 1 NO-UNDO.

DEFINE VARIABLE svBeforeValueFilter AS CHARACTER FORMAT "X(256)":U 
     LABEL "Before Value" 
     VIEW-AS FILL-IN 
     SIZE 70.4 BY 1 NO-UNDO.

DEFINE VARIABLE svEndDate AS DATE FORMAT "99/99/9999":U 
     LABEL "To Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Enter To Date" NO-UNDO.

DEFINE VARIABLE svStartDate AS DATE FORMAT "99/99/9999":U 
     LABEL "From Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Enter From Date" NO-UNDO.

DEFINE BUTTON btnPrint 
     IMAGE-UP FILE "Graphics/32x32/printer.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8.4 BY 2 TOOLTIP "Print"
     FONT 4.

DEFINE BUTTON btnRestore 
     IMAGE-UP FILE "Graphics/32x32/undo_32.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8.4 BY 2 TOOLTIP "Restore"
     FONT 4.

DEFINE BUTTON btnStack 
     IMAGE-UP FILE "Graphics/32x32/text_tree.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8.4 BY 2 TOOLTIP "Program Trace"
     FONT 4.

DEFINE VARIABLE svSortByDtl AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
      VIEW-AS TEXT 
     SIZE 25 BY .62
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE svSortByHdr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
      VIEW-AS TEXT 
     SIZE 24 BY .62
     BGCOLOR 14  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY AuditDetail FOR 
      AuditDtl SCROLLING.

DEFINE QUERY AuditHeader FOR 
      AuditHdr, 
      AuditDtl SCROLLING.

DEFINE QUERY AuditSearch FOR 
      AuditHdr SCROLLING.

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
    WITH NO-ROW-MARKERS SEPARATORS SIZE 124 BY 18.1
         FGCOLOR 1 
         TITLE FGCOLOR 1 "Audit Detail".

DEFINE BROWSE AuditHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS AuditHeader C-Win _STRUCTURED
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
      AuditHdr.AuditStackID FORMAT "->,>>>,>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 96 BY 23.1
         FGCOLOR 1 
         TITLE FGCOLOR 1 "Audit Header".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnRestore AT ROW 9.33 COL 102 HELP
          "Print" WIDGET-ID 284
     AuditHeader AT ROW 6.48 COL 1 WIDGET-ID 200
     AuditDetail AT ROW 6.48 COL 115 WIDGET-ID 300
     btnStack AT ROW 6.24 COL 106 HELP
          "Click to View Program Stack Trace" WIDGET-ID 282
     btnPrint AT ROW 6.24 COL 97 HELP
          "Print" WIDGET-ID 280
     svSortByHdr AT ROW 5.76 COL 68 COLON-ALIGNED WIDGET-ID 2
     svSortByDtl AT ROW 5.76 COL 207 COLON-ALIGNED WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 238.2 BY 28.57 WIDGET-ID 100.

DEFINE FRAME AuditView
     AuditHdr.AuditDB AT ROW 1.24 COL 6 COLON-ALIGNED WIDGET-ID 16
          LABEL "DB"
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
          BGCOLOR 15 
     AuditHdr.AuditType AT ROW 1.24 COL 22 COLON-ALIGNED WIDGET-ID 22
          LABEL "Type"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
          BGCOLOR 15 
     AuditHdr.AuditID AT ROW 1.24 COL 46 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
          BGCOLOR 15 
     AuditHdr.AuditUser AT ROW 1.24 COL 75 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
          BGCOLOR 15 
     AuditHdr.AuditDateTime AT ROW 1.24 COL 106 COLON-ALIGNED WIDGET-ID 14
          LABEL "Date / Time"
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
          BGCOLOR 15 
     AuditHdr.AuditTable AT ROW 2.43 COL 6 COLON-ALIGNED WIDGET-ID 20
          LABEL "Table"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     AuditDtl.AuditIdxField AT ROW 2.43 COL 30 COLON-ALIGNED WIDGET-ID 12
          LABEL "Idx" FORMAT "Yes/No"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
          BGCOLOR 15 
     AuditDtl.AuditBeforeValue AT ROW 2.43 COL 46 COLON-ALIGNED WIDGET-ID 4
          LABEL "Before" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 94 BY 1
          BGCOLOR 15 
     AuditDtl.AuditField AT ROW 3.62 COL 6 COLON-ALIGNED WIDGET-ID 8
          LABEL "Field"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     AuditDtl.AuditExtent AT ROW 3.62 COL 30 COLON-ALIGNED WIDGET-ID 6
          LABEL "Ext"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
          BGCOLOR 15 
     AuditDtl.AuditAfterValue AT ROW 3.62 COL 46 COLON-ALIGNED WIDGET-ID 2
          LABEL "After" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 94 BY 1
          BGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 97 ROW 24.57
         SIZE 142 BY 4.81
         TITLE BGCOLOR 15 "Audit Detail View" WIDGET-ID 400.

DEFINE FRAME AuditSearch
     svType AT ROW 1.24 COL 8 COLON-ALIGNED HELP
          "Select Audit Type Filter" WIDGET-ID 6
     svStartDate AT ROW 1.24 COL 36 COLON-ALIGNED HELP
          "Enter From Date" WIDGET-ID 20
     btnCalendar-1 AT ROW 1.24 COL 54 WIDGET-ID 272
     btnAuditTables AT ROW 2.67 COL 118 HELP
          "Click to Access Tables to Audit" WIDGET-ID 288
     svStartDateOption AT ROW 1.24 COL 59 HELP
          "Select Start Date Option" NO-LABEL WIDGET-ID 74
     svDB AT ROW 1.24 COL 89 COLON-ALIGNED HELP
          "Select Audit DB Filter" WIDGET-ID 14
     maxRows AT ROW 1.24 COL 128 COLON-ALIGNED WIDGET-ID 290
     AuditHdr.AuditKey AT ROW 1.24 COL 161.6 COLON-ALIGNED WIDGET-ID 26
          LABEL "Audit Key" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 70.4 BY 1
          BGCOLOR 15 
     svUser AT ROW 2.43 COL 8 COLON-ALIGNED HELP
          "Select User Filter" WIDGET-ID 12
     svEndDate AT ROW 2.43 COL 36 COLON-ALIGNED HELP
          "Enter To Date" WIDGET-ID 22
     btnCalendar-2 AT ROW 2.43 COL 54 WIDGET-ID 274
     svEndDateOption AT ROW 2.43 COL 59 HELP
          "Select End Date Option" NO-LABEL WIDGET-ID 70
     svTable AT ROW 2.43 COL 89 COLON-ALIGNED HELP
          "Select Audit Table Filter" WIDGET-ID 16
     svBeforeValueFilter AT ROW 2.43 COL 161.6 COLON-ALIGNED HELP
          "Enter Before Value to Filter" WIDGET-ID 36
     svField AT ROW 3.62 COL 89 COLON-ALIGNED HELP
          "Select Audit Field Filter" WIDGET-ID 18
     svAfterValueFilter AT ROW 3.62 COL 161.6 COLON-ALIGNED HELP
          "Enter After Value to Filter" WIDGET-ID 38
     btnAfterValueFilterClear AT ROW 3.62 COL 234 HELP
          "Click to Clear After Value Filter" WIDGET-ID 42
     btnBeforeValueFilterClear AT ROW 2.43 COL 234 HELP
          "Click to Clear Before Value Filter" WIDGET-ID 40
     btnClear AT ROW 2.67 COL 127 HELP
          "Click to Clear Filters" WIDGET-ID 284
     btnSearch AT ROW 2.67 COL 136 HELP
          "Click to Apply Filter Selections" WIDGET-ID 286
     btnFilterAfterValue AT ROW 3.62 COL 145 HELP
          "Select to Filter by After Value" WIDGET-ID 34
     btnFilterAuditKey AT ROW 1.24 COL 145 HELP
          "Select to Filter by Audit Key" WIDGET-ID 28
     btnFilterBeforeValue AT ROW 2.43 COL 145 HELP
          "Select to Filter by Before Value" WIDGET-ID 32
     btnHistory AT ROW 1.24 COL 234 HELP
          "Click to View History" WIDGET-ID 30
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 238 BY 4.76
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
                                                                        */
/* SETTINGS FOR FILL-IN AuditHdr.AuditKey IN FRAME AuditSearch
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
ASSIGN 
       AuditHdr.AuditKey:READ-ONLY IN FRAME AuditSearch        = TRUE.

/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME AuditSearch
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-2 IN FRAME AuditSearch
   3                                                                    */
/* SETTINGS FOR FILL-IN maxRows IN FRAME AuditSearch
   1                                                                    */
ASSIGN 
       maxRows:PRIVATE-DATA IN FRAME AuditSearch     = 
                "NoUserPrint".

ASSIGN 
       svAfterValueFilter:PRIVATE-DATA IN FRAME AuditSearch     = 
                "parameter".

ASSIGN 
       svBeforeValueFilter:PRIVATE-DATA IN FRAME AuditSearch     = 
                "parameter".

/* SETTINGS FOR COMBO-BOX svDB IN FRAME AuditSearch
   1                                                                    */
ASSIGN 
       svDB:PRIVATE-DATA IN FRAME AuditSearch     = 
                "parameter".

/* SETTINGS FOR FILL-IN svEndDate IN FRAME AuditSearch
   1                                                                    */
ASSIGN 
       svEndDate:PRIVATE-DATA IN FRAME AuditSearch     = 
                "parameter".

/* SETTINGS FOR COMBO-BOX svEndDateOption IN FRAME AuditSearch
   ALIGN-L 1                                                            */
ASSIGN 
       svEndDateOption:PRIVATE-DATA IN FRAME AuditSearch     = 
                "parameter".

/* SETTINGS FOR COMBO-BOX svField IN FRAME AuditSearch
   1                                                                    */
ASSIGN 
       svField:PRIVATE-DATA IN FRAME AuditSearch     = 
                "parameter".

/* SETTINGS FOR FILL-IN svStartDate IN FRAME AuditSearch
   1                                                                    */
ASSIGN 
       svStartDate:PRIVATE-DATA IN FRAME AuditSearch     = 
                "parameter".

/* SETTINGS FOR COMBO-BOX svStartDateOption IN FRAME AuditSearch
   ALIGN-L 1                                                            */
ASSIGN 
       svStartDateOption:PRIVATE-DATA IN FRAME AuditSearch     = 
                "parameter".

/* SETTINGS FOR COMBO-BOX svTable IN FRAME AuditSearch
   1                                                                    */
ASSIGN 
       svTable:PRIVATE-DATA IN FRAME AuditSearch     = 
                "parameter".

/* SETTINGS FOR COMBO-BOX svType IN FRAME AuditSearch
   1                                                                    */
ASSIGN 
       svType:PRIVATE-DATA IN FRAME AuditSearch     = 
                "parameter".

/* SETTINGS FOR COMBO-BOX svUser IN FRAME AuditSearch
   1                                                                    */
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
       AuditHeader:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 2500
       AuditHeader:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR BUTTON btnRestore IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       btnRestore:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

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
     _Options          = "NO-LOCK SORTBY-PHRASE"
     _Where[1]         = "AuditDtl.AuditID EQ AuditHdr.AuditID
AND (AuditDtl.AuditField EQ svField
OR svField EQ ""ALL"")
AND (AuditDtl.AuditBeforeValue BEGINS cBeforeValueFilter
OR cBeforeValueFilter EQ """")
AND (AuditDtl.AuditAfterValue BEGINS cAfterValueFilter
OR cAfterValueFilter EQ """")"
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
     _Query            is OPENED
*/  /* BROWSE AuditDetail */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE AuditHeader
/* Query rebuild information for BROWSE AuditHeader
     _TblList          = "audit.AuditHdr,audit.AuditDtl OF audit.AuditHdr"
     _Options          = "NO-LOCK SORTBY-PHRASE"
     _TblOptList       = ", FIRST"
     _Where[1]         = "AuditHdr.AuditDateTime GE dtStartDateTime
AND AuditHdr.AuditDateTime LE dtEndDateTime
AND (AuditHdr.AuditType EQ svType OR svType EQ ""ALL"")
AND (AuditHdr.AuditUser EQ svUser OR svUser EQ ""ALL"")
AND (AuditHdr.AuditDB EQ svDB OR svDB EQ ""ALL"")
AND (AuditHdr.AuditTable EQ svTable OR svTable EQ ""ALL"")
AND (AuditHdr.AuditKey EQ cAuditKeyFilter
OR lAuditKeyFilter EQ FALSE)
"
     _Where[2]         = "(AuditDtl.AuditField EQ svField
OR svField EQ ""ALL"")
AND (AuditDtl.AuditBeforeValue BEGINS cBeforeValueFilter
OR cBeforeValueFilter EQ """")
AND (AuditDtl.AuditAfterValue BEGINS cAfterValueFilter
OR cAfterValueFilter EQ """")"
     _FldNameList[1]   > audit.AuditHdr.AuditID
"AuditHdr.AuditID" ? ? "integer" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > audit.AuditHdr.AuditType
"AuditHdr.AuditType" ? ? "character" ? ? ? 14 ? ? no ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > audit.AuditHdr.AuditDateTime
"AuditHdr.AuditDateTime" "Date / Time" ? "datetime" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > audit.AuditHdr.AuditDB
"AuditHdr.AuditDB" "DB" "x(6)" "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > audit.AuditHdr.AuditTable
"AuditHdr.AuditTable" "Table / Prgm" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > audit.AuditHdr.AuditUser
"AuditHdr.AuditUser" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > audit.AuditHdr.AuditKey
"AuditHdr.AuditKey" ? "x(40)" "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   = audit.AuditHdr.AuditStackID
     _Query            is OPENED
*/  /* BROWSE AuditHeader */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME AuditSearch
/* Query rebuild information for FRAME AuditSearch
     _TblList          = "Audit.AuditHdr"
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
    IF {&BROWSE-NAME}:CURRENT-COLUMN:NAME NE ? THEN DO:
        cColumnLabel = BROWSE AuditDetail:CURRENT-COLUMN:NAME.
        IF cColumnLabel EQ cSaveLabel THEN
        lAscending = NOT lAscending.
        ASSIGN
            lHeaderSorting = NO
            cSaveLabel = cColumnLabel
            svSortByDtl:SCREEN-VALUE = BROWSE AuditDetail:CURRENT-COLUMN:LABEL + " "
                                     + STRING(lAscending,"Ascending/Descending")
            .
        RUN pReopenBrowse.
    END.
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
        DISPLAY
            AuditHdr.AuditKey
                WITH FRAME AuditSearch.
    END. /* if avail */
    ELSE DO:
        MESSAGE
            "No Audit History Records Found."
        VIEW-AS ALERT-BOX.
/*        APPLY 'CHOOSE':U TO btnClear.*/
/*        RUN pReopenBrowse.           */
/*        {&OPEN-QUERY-AuditDetail}    */
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME AuditHeader
&Scoped-define SELF-NAME AuditHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AuditHeader C-Win
ON START-SEARCH OF AuditHeader IN FRAME DEFAULT-FRAME /* Audit Header */
DO:
    IF {&BROWSE-NAME}:CURRENT-COLUMN:NAME NE ? THEN DO:
        cColumnLabel = BROWSE AuditHeader:CURRENT-COLUMN:NAME.
        IF cColumnLabel EQ cSaveLabel THEN
        lAscending = NOT lAscending.
        ASSIGN
            lHeaderSorting = YES
            cSaveLabel = cColumnLabel
            svSortByHdr:SCREEN-VALUE = BROWSE AuditHeader:CURRENT-COLUMN:LABEL + " "
                                     + STRING(lAscending,"Ascending/Descending")
            .
        RUN pReopenBrowse.
    END.
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
        btnRestore:HIDDEN    = AuditHdr.AuditType NE "DELETE"
        btnRestore:SENSITIVE = AuditHdr.AuditType EQ "DELETE"
        .
    {&OPEN-QUERY-AuditDetail}
    APPLY 'VALUE-CHANGED':U TO BROWSE AuditDetail.
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
        cAfterValueFilter = ""
        svAfterValueFilter:BGCOLOR = 15
        .
    APPLY 'CHOOSE':U TO btnSearch.
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
        cBeforeValueFilter = ""
        svBeforeValueFilter:BGCOLOR = 15
        .
    APPLY 'CHOOSE':U TO btnSearch.
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


&Scoped-define SELF-NAME btnClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClear C-Win
ON CHOOSE OF btnClear IN FRAME AuditSearch /* Clear */
DO:
    ASSIGN
        {&WINDOW-NAME}:TITLE = "Audit History"
        svType:SCREEN-VALUE  = "All"
        svType
        svUser:SCREEN-VALUE  = "All"
        svUser
        svDB:SCREEN-VALUE    = "All"
        svDB
        svTable:SCREEN-VALUE = "All"
        svTable
        svField:SCREEN-VALUE = "All"
        svField
        cAuditKeyFilter      = ""
        lAuditKeyFilter      = NO
        AuditHdr.AuditKey:BGCOLOR   = 15
        lBeforeValueFilter   = NO
        cBeforeValueFilter   = ""
        svBeforeValueFilter:BGCOLOR = 15
        lAfterValueFilter    = NO
        cAfterValueFilter    = ""
        svAfterValueFilter:BGCOLOR  = 15
        .
    RUN pGetFilterValues ("ALL").
    RUN pGetFilterValues ("TABLE").
    RUN pGetFilterValues ("FIELD").
    APPLY 'VALUE-CHANGED':U TO svStartDateOption.
    APPLY 'VALUE-CHANGED':U TO svEndDateOption.
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
        cAfterValueFilter = IF lAfterValueFilter THEN svAfterValueFilter ELSE ""
        svAfterValueFilter:BGCOLOR = IF lAfterValueFilter THEN 14 ELSE 15
        .
    APPLY 'CHOOSE':U TO btnSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFilterAuditKey
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFilterAuditKey C-Win
ON CHOOSE OF btnFilterAuditKey IN FRAME AuditSearch /* Filter */
DO:
    ASSIGN
        lAuditKeyFilter = NOT lAuditKeyFilter
        cAuditKeyFilter = IF lAuditKeyFilter THEN AuditHdr.AuditKey ELSE ""
        AuditHdr.AuditKey:BGCOLOR = IF lAuditKeyFilter THEN 14 ELSE 15
        .
    APPLY 'CHOOSE':U TO btnSearch.
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
        cBeforeValueFilter = IF lBeforeValueFilter THEN svBeforeValueFilter ELSE ""
        svBeforeValueFilter:BGCOLOR = IF lBeforeValueFilter THEN 14 ELSE 15
        .
    APPLY 'CHOOSE':U TO btnSearch.
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


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrint C-Win
ON CHOOSE OF btnPrint IN FRAME DEFAULT-FRAME
DO:
    RUN pUserPrint.
    RUN AOA/AuditHist.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRestore
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRestore C-Win
ON CHOOSE OF btnRestore IN FRAME DEFAULT-FRAME
DO:
    RUN pRestore.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME AuditSearch
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
        .
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME btnStack
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStack C-Win
ON CHOOSE OF btnStack IN FRAME DEFAULT-FRAME
DO:
    IF AVAILABLE AuditHdr THEN
    RUN system\AuditStack.w (AuditHdr.AuditStackID).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME AuditSearch
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
  ASSIGN
    {&SELF-NAME}
    svTable:SCREEN-VALUE = "All"
    svTable
    svField:SCREEN-VALUE = "All"
    svField
    .
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
    ASSIGN
        {&SELF-NAME}
        dtEndDateTime = DATETIME(STRING({&SELF-NAME},"99/99/9999") + " 23:59:59")
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svEndDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svEndDateOption C-Win
ON VALUE-CHANGED OF svEndDateOption IN FRAME AuditSearch
DO:
    {aoa/includes/tDateOption.i &dateObject=svEndDate &btnCalendar=2}
    APPLY 'LEAVE':U TO svEndDate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svField
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svField C-Win
ON VALUE-CHANGED OF svField IN FRAME AuditSearch /* Field */
DO:
  ASSIGN {&SELF-NAME}.
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
    ASSIGN
        {&SELF-NAME}
        dtStartDateTime = DATETIME(STRING({&SELF-NAME},"99/99/9999") + " 00:00:00")
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStartDateOption
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStartDateOption C-Win
ON VALUE-CHANGED OF svStartDateOption IN FRAME AuditSearch
DO:
    {AOA/includes/tDateOption.i &dateObject=svStartDate &btnCalendar=1}
    APPLY 'LEAVE':U TO svStartDate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svTable C-Win
ON VALUE-CHANGED OF svTable IN FRAME AuditSearch /* Table */
DO:
  ASSIGN
    {&SELF-NAME}
    svField:SCREEN-VALUE = "All"
    svField
    .
    RUN pGetFilterValues ("FIELD").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svType C-Win
ON VALUE-CHANGED OF svType IN FRAME AuditSearch /* Type */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svUser
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svUser C-Win
ON VALUE-CHANGED OF svUser IN FRAME AuditSearch /* User ID */
DO:
  ASSIGN {&SELF-NAME}.
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
    RUN pGetFilterValues ("INIT").
    RUN pGetFilterValues ("ALL").
    RUN pGetFilterValues ("TABLE").
    RUN pGetFilterValues ("FIELD").
    &IF DEFINED(AuditHistory) NE 0 &THEN
    /* scop def set in system/CallAudit.p         */
    /* invoked by CTRL-A from viewers or browsers */
    ASSIGN
        lAuditKeyFilter = YES
        cAuditKeyFilter = fAuditKey(iphTable,cIdxFlds)
        AuditHdr.AuditKey:BGCOLOR = 14
        {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE
                             + " [Key: "
                             + cAuditKeyFilter + "]"
                             + " Called from "
                             + ipcType + ": "
                             + ipcProgramName
                             .
    &ELSE
    RUN pGetSettings.
    &ENDIF
    APPLY 'VALUE-CHANGED':U TO svStartDateOption.
    APPLY 'VALUE-CHANGED':U TO svEndDateOption.
    {&OPEN-QUERY-AuditHeader}
    APPLY 'VALUE-CHANGED':U TO BROWSE AuditHeader.
  END. /* if lcontinue */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
  IF NOT lContinue THEN
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

&SCOPED-DEFINE FilterFrame AuditSearch
{AOA/includes/pGetAuditQueryFilters.i}

&Scoped-define sdBrowseName AuditHeader
{methods/sortByProc.i "pByAuditID" "AuditHdr.AuditID"}
{methods/sortByProc.i "pByAuditType" "AuditHdr.AuditType"}
{methods/sortByProc.i "pByAuditDateTime" "AuditHdr.AuditDateTime"}
{methods/sortByProc.i "pByAuditDB" "AuditHdr.AuditDB"}
{methods/sortByProc.i "pByAuditTable" "AuditHdr.AuditTable"}
{methods/sortByProc.i "pByAuditUser" "AuditHdr.AuditUser"}
{methods/sortByProc.i "pByAuditKey" "AuditHdr.AuditKey"}

&Scoped-define sdBrowseName AuditDetail
{methods/sortByProc.i "pByAuditIdxField" "AuditDtl.AuditIdxField"}
{methods/sortByProc.i "pByAuditField" "AuditDtl.AuditField BY AuditDtl.AuditExtent"}
{methods/sortByProc.i "pByAuditExtent" "AuditDtl.AuditExtent"}
{methods/sortByProc.i "pByAuditBeforeValue" "AuditDtl.AuditBeforeValue"}
{methods/sortByProc.i "pByAuditAfterValue" "AuditDtl.AuditAfterValue"}

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
  DISPLAY svType svStartDate svStartDateOption svDB maxRows svUser svEndDate 
          svEndDateOption svTable svBeforeValueFilter svField svAfterValueFilter 
      WITH FRAME AuditSearch IN WINDOW C-Win.
  IF AVAILABLE AuditHdr THEN 
    DISPLAY AuditHdr.AuditKey 
      WITH FRAME AuditSearch IN WINDOW C-Win.
  ENABLE svType svStartDate btnCalendar-1 btnAuditTables svStartDateOption svDB 
         maxRows svUser svEndDate btnCalendar-2 svEndDateOption svTable 
         svBeforeValueFilter svField svAfterValueFilter 
         btnAfterValueFilterClear btnBeforeValueFilterClear btnClear btnSearch 
         btnFilterAfterValue btnFilterAuditKey btnFilterBeforeValue btnHistory 
      WITH FRAME AuditSearch IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-AuditSearch}
  DISPLAY svSortByHdr svSortByDtl 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE AuditHeader AuditDetail btnStack btnPrint svSortByHdr svSortByDtl 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  IF AVAILABLE AuditDtl THEN 
    DISPLAY AuditDtl.AuditIdxField AuditDtl.AuditBeforeValue AuditDtl.AuditField 
          AuditDtl.AuditExtent AuditDtl.AuditAfterValue 
      WITH FRAME AuditView IN WINDOW C-Win.
  IF AVAILABLE AuditHdr THEN 
    DISPLAY AuditHdr.AuditDB AuditHdr.AuditType AuditHdr.AuditID 
          AuditHdr.AuditUser AuditHdr.AuditDateTime AuditHdr.AuditTable 
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
            WHERE  AuditHdr.AuditDateTime  GE dtStartDateTime
              AND  AuditHdr.AuditDateTime  LE dtEndDateTime
              AND (AuditHdr.AuditType      EQ svType  OR svType  EQ "All")
              AND (AuditHdr.AuditUser      EQ svUser  OR svUser  EQ "All")
              AND (AuditHdr.AuditDB        EQ svDB    OR svDB    EQ "All")
              AND (AuditHdr.AuditTable     EQ svTable OR svTable EQ "All")
              AND  AuditHdr.AuditKey       EQ cAuditKeyFilter,
             EACH  AuditDtl OF AuditHdr NO-LOCK
            WHERE (AuditDtl.AuditField EQ svField
               OR svField EQ "All")
              AND (AuditDtl.AuditBeforeValue BEGINS cBeforeValueFilter
               OR cBeforeValueFilter EQ "")
              AND (AuditDtl.AuditAfterValue  BEGINS cAfterValueFilter
               OR cAfterValueFilter EQ "")
            INDEXED-REPOSITION
            {&MAX-ROWS}
            .
    APPLY 'VALUE-CHANGED':U TO BROWSE AuditHeader.
    
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
    DO WHILE VALID-HANDLE(hChild):
        IF hChild:NAME NE ? AND (hChild:SENSITIVE OR hChild:TYPE EQ "COMBO-BOX") THEN DO:
            DO idx = 1 TO EXTENT(user-print.field-name):
                IF TRIM(user-print.field-name[idx]) EQ hChild:NAME THEN DO:
                    IF hChild:PRIVATE-DATA EQ "NoUserPrint" THEN LEAVE.
                    hChild:SCREEN-VALUE = user-print.field-value[idx].
                    LEAVE.
                END. /* found screen object */
            END. /* do idx */
        END. /* name <> ? */
        hChild = hChild:NEXT-SIBLING.
    END. /* do while */
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
        .
&IF DEFINED(FWD-VERSION) > 0 &THEN
    open-mime-resource "text/plain" "file:///c:\tmp\AuditHistory.txt" false.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReopenBrowse C-Win 
PROCEDURE pReopenBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    SESSION:SET-WAIT-STATE("General").
    CASE cColumnLabel:
        WHEN "AuditType" THEN
        RUN pByAuditType.
        WHEN "AuditDateTime" THEN
        RUN pByAuditDateTime.
        WHEN "AuditDB" THEN
        RUN pByAuditDB.
        WHEN "AuditTable" THEN
        RUN pByAuditTable.
        WHEN "AuditUser" THEN
        RUN pByAuditUser.
        WHEN "AuditIdxField" THEN
        RUN pByAuditIdxField.
        WHEN "AuditField" THEN
        RUN pByAuditField.
        WHEN "AuditKey" THEN
        RUN pByAuditKey.
        WHEN "AuditExtent" THEN
        RUN pByAuditExtent.
        WHEN "AuditBeforeValue" THEN
        RUN pByAuditBeforeValue.
        WHEN "AuditAfterValue" THEN
        RUN pByAuditAfterValue.
        OTHERWISE
        RUN pByAuditID.
    END CASE.
    SESSION:SET-WAIT-STATE("").
    IF lHeaderSorting THEN
    APPLY 'VALUE-CHANGED':U TO BROWSE AuditHeader.

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
    END. /* do with */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

