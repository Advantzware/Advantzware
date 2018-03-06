&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: columns.w

  Description: allows setting resource popup column order

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 7.10.2004
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE dynColumns
&SCOPED-DEFINE useTtbl ttblJob
&SCOPED-DEFINE designMode

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ipHeight AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipWidth AS INTEGER NO-UNDO.

DEFINE OUTPUT PARAMETER opRunAgain AS LOGICAL NO-UNDO.

opRunAgain = NO.

/* Local Variable Definitions ---                                       */

{schedule/scopDir.i}
{{&includes}/defBoard.i}
{{&includes}/filterVars.i}
{{&viewers}/includes/browseDef.i}
{{&viewers}/includes/sharedVars.i}
{{&includes}/ttblJob.i}
{{&includes}/sharedVars.i}
{{&includes}/rptTables.i}

DEFINE VARIABLE currentColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE moveColumns AS LOGICAL NO-UNDO.
DEFINE VARIABLE lineCoord AS INTEGER NO-UNDO EXTENT 7.

DEFINE BUFFER bBrowseColumn FOR browseColumn.

DEFINE TEMP-TABLE colOrder NO-UNDO
  FIELD colOrder AS INTEGER
  FIELD colLocked AS LOGICAL
  FIELD colHidden AS LOGICAL
  FIELD browseColumnRowID AS ROWID
    INDEX colOrder IS PRIMARY colOrder.

{{&includes}/lockWindowUpdate.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME browseJob

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttblJob browseRptFields

/* Definitions for BROWSE browseJob                                     */
&Scoped-define FIELDS-IN-QUERY-browseJob ttblJob.jobSequence ttblJob.job ttblJob.resource ttblJob.resourceSequence ttblJob.jobCompleted   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseJob   
&Scoped-define SELF-NAME browseJob
&Scoped-define QUERY-STRING-browseJob FOR EACH ttblJob NO-LOCK
&Scoped-define OPEN-QUERY-browseJob OPEN QUERY {&SELF-NAME} FOR EACH ttblJob NO-LOCK.
&Scoped-define TABLES-IN-QUERY-browseJob ttblJob
&Scoped-define FIRST-TABLE-IN-QUERY-browseJob ttblJob


/* Definitions for BROWSE browserFilterFields                           */
&Scoped-define FIELDS-IN-QUERY-browserFilterFields browseRptFields.fieldLabel browseRptFields.fieldName browseRptFields.filterField   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browserFilterFields browseRptFields.filterField   
&Scoped-define ENABLED-TABLES-IN-QUERY-browserFilterFields browseRptFields
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-browserFilterFields browseRptFields
&Scoped-define SELF-NAME browserFilterFields
&Scoped-define QUERY-STRING-browserFilterFields FOR EACH browseRptFields   WHERE browseRptFields.rptID NE ''     AND ID BEGINS browseRptFields.rptID
&Scoped-define OPEN-QUERY-browserFilterFields OPEN QUERY {&SELF-NAME} FOR EACH browseRptFields   WHERE browseRptFields.rptID NE ''     AND ID BEGINS browseRptFields.rptID.
&Scoped-define TABLES-IN-QUERY-browserFilterFields browseRptFields
&Scoped-define FIRST-TABLE-IN-QUERY-browserFilterFields browseRptFields


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-browseJob}

/* Definitions for FRAME layoutFrame                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-layoutFrame ~
    ~{&OPEN-QUERY-browserFilterFields}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnMove btnHidden btnSaveColumns ~
btnResetColumns browseJob reportName reportFormat reportAltName copyFormat ~
layoutFields btnTestLayout btnSaveLayouts btnResetLayout btnDelete btnCopy ~
excludeLayout 
&Scoped-Define DISPLAYED-OBJECTS selectedColumn reportName reportFormat ~
reportAltName copyFormat layoutFields excludeLayout linePos columnPos 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 btnTestLayout 
&Scoped-define List-2 btnTestLayout 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCopy 
     IMAGE-UP FILE "schedule/images/copy.bmp":U
     LABEL "" 
     SIZE 5 BY 1.1 TOOLTIP "Copy".

DEFINE BUTTON btnDelete 
     IMAGE-UP FILE "schedule/images/cancel.bmp":U
     LABEL "" 
     SIZE 5 BY 1.1 TOOLTIP "Delete Layout".

DEFINE BUTTON btnHidden 
     LABEL "&Hide/UnHide" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnMove 
     LABEL "&Start Move" 
     SIZE 13 BY 1.14.

DEFINE BUTTON btnResetColumns AUTO-GO 
     IMAGE-UP FILE "schedule/images/rollback.bmp":U
     LABEL "" 
     SIZE 5 BY 1.14 TOOLTIP "Reset from Last Save".

DEFINE BUTTON btnResetLayout AUTO-GO 
     IMAGE-UP FILE "schedule/images/rollback.bmp":U
     LABEL "" 
     SIZE 5 BY 1.1 TOOLTIP "Reset from Last Save".

DEFINE BUTTON btnSaveColumns AUTO-GO 
     IMAGE-UP FILE "schedule/images/commit.bmp":U
     LABEL "" 
     SIZE 5 BY 1.14 TOOLTIP "Save Columns".

DEFINE BUTTON btnSaveLayouts 
     IMAGE-UP FILE "schedule/images/commit.bmp":U
     LABEL "" 
     SIZE 5 BY 1.1 TOOLTIP "Save Layout".

DEFINE BUTTON btnTestLayout 
     IMAGE-UP FILE "schedule/images/print.bmp":U
     LABEL "" 
     SIZE 5 BY 1.1 TOOLTIP "Test Layout (Preview)".

DEFINE VARIABLE copyFormat AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Copy Format" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "","1","2","3","4","5","6","7","8","9" 
     DROP-DOWN-LIST
     SIZE 7 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE layoutFields AS CHARACTER FORMAT "X(256)":U 
     LABEL "Field&s" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 100
     LIST-ITEM-PAIRS "Empty","Empty"
     DROP-DOWN-LIST
     SIZE 17 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE reportFormat AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Format" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "","1","2","3","4","5","6","7","8","9" 
     DROP-DOWN-LIST
     SIZE 7 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE reportName AS CHARACTER FORMAT "X(256)":U INITIAL "jobByResource" 
     LABEL "&Report" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 30
     LIST-ITEM-PAIRS "Job By Resource Report","jobByResource"
     DROP-DOWN-LIST
     SIZE 31 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE columnPos AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Col" 
      VIEW-AS TEXT 
     SIZE 4 BY .62
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE linePos AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Line" 
      VIEW-AS TEXT 
     SIZE 3 BY .62
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE reportAltName AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Name" 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE selectedColumn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Column" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 17 BY 1.14
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 17 BY 1.14
     BGCOLOR 12 .

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 18 BY 1.14
     BGCOLOR 14 .

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 22 BY 1.14
     BGCOLOR 15 .

DEFINE VARIABLE excludeLayout AS LOGICAL INITIAL no 
     LABEL "&Exclude" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .81 NO-UNDO.

DEFINE BUTTON btnDown 
     IMAGE-UP FILE "schedule/images/down.bmp":U
     LABEL "&DN" 
     SIZE 5 BY 1.1 TOOLTIP "Move Current Column and Row Down (Alt-D)".

DEFINE BUTTON btnResetFilterFields AUTO-GO 
     IMAGE-UP FILE "schedule/images/rollback.bmp":U
     LABEL "" 
     SIZE 5 BY 1.14 TOOLTIP "Reset from Last Save".

DEFINE BUTTON btnSaveFilterFields 
     IMAGE-UP FILE "schedule/images/commit.bmp":U
     LABEL "" 
     SIZE 5 BY 1.14 TOOLTIP "Save Filter Fields".

DEFINE BUTTON btnTestExcel 
     IMAGE-UP FILE "schedule/images/excel.bmp":U
     LABEL "" 
     SIZE 9 BY 2.14 TOOLTIP "Test Excel (Preview)".

DEFINE BUTTON btnUp 
     IMAGE-UP FILE "schedule/images/up.bmp":U
     LABEL "&UP" 
     SIZE 5 BY 1.1 TOOLTIP "Move Current Column and Row Up (Alt-U)".

DEFINE VARIABLE ruler-1 AS CHARACTER FORMAT "X(256)":U INITIAL "    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    1    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    |    2    |    |    |    |    |    |    |    |" 
      VIEW-AS TEXT 
     SIZE 154.6 BY .71
     BGCOLOR 0 FGCOLOR 15 FONT 2 NO-UNDO.

DEFINE VARIABLE ruler-2 AS CHARACTER FORMAT "X(256)":U INITIAL "    |    1    |    2    |    3    |    4    |    5    |    6    |    7    |    8    |    9    |    0    |    1    |    2    |    3    |    4    |    5    |    6    |    7    |    8    |    9    |    0    |    1    |    2    |    3    |    4" 
      VIEW-AS TEXT 
     SIZE 154.6 BY .71
     BGCOLOR 0 FGCOLOR 15 FONT 2 NO-UNDO.

DEFINE VARIABLE ruler-3 AS CHARACTER FORMAT "X(256)":U INITIAL "123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890" 
      VIEW-AS TEXT 
     SIZE 154.6 BY .62
     BGCOLOR 0 FGCOLOR 15 FONT 2 NO-UNDO.

DEFINE RECTANGLE availableFields
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 154.6 BY 1.91
     BGCOLOR 7 .

DEFINE RECTANGLE excelRect
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 83 BY 5
     BGCOLOR 2 .

DEFINE RECTANGLE line-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 154.6 BY 1.43.

DEFINE RECTANGLE line-2
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 154.6 BY 1.43.

DEFINE RECTANGLE line-3
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 154.6 BY 1.43.

DEFINE RECTANGLE line-4
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 154.6 BY 1.43.

DEFINE RECTANGLE line-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 154.6 BY 1.43.

DEFINE RECTANGLE line-6
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 154.6 BY 1.43.

DEFINE VARIABLE excelOrder AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "Empty","Empty" 
     SIZE 35.2 BY 3.81 NO-UNDO.

DEFINE VARIABLE unAssigned AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SORT SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "Empty","Empty" 
     SIZE 35.2 BY 3.81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browseJob FOR 
      ttblJob SCROLLING.

DEFINE QUERY browserFilterFields FOR 
      browseRptFields SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browseJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseJob Dialog-Frame _FREEFORM
  QUERY browseJob DISPLAY
      ttblJob.jobSequence LABEL-BGCOLOR 12
  ttblJob.job LABEL-BGCOLOR 12
  ttblJob.resource LABEL-BGCOLOR 12
  ttblJob.resourceSequence LABEL-BGCOLOR 12
  ttblJob.jobCompleted LABEL-BGCOLOR 12
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 156 BY 4.76
         BGCOLOR 15 
         TITLE BGCOLOR 15 "Browser Columns" ROW-HEIGHT-CHARS .52.

DEFINE BROWSE browserFilterFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browserFilterFields Dialog-Frame _FREEFORM
  QUERY browserFilterFields DISPLAY
      browseRptFields.fieldLabel LABEL 'Label' FORMAT 'X(20)'
 browseRptFields.fieldName LABEL 'Name' FORMAT 'X(20)'
 browseRptFields.filterField LABEL 'Filter'
ENABLE
 browseRptFields.filterField
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 51 BY 5
         TITLE "Filter Fields".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnMove AT ROW 1.24 COL 2
     btnHidden AT ROW 1.24 COL 57
     btnSaveColumns AT ROW 1.24 COL 73 HELP
          "Click to Save Columns"
     btnResetColumns AT ROW 1.24 COL 78 HELP
          "Reset Columns"
     selectedColumn AT ROW 1.33 COL 22 COLON-ALIGNED
     browseJob AT ROW 2.67 COL 2
     reportName AT ROW 7.67 COL 8 COLON-ALIGNED
     reportFormat AT ROW 7.67 COL 47 COLON-ALIGNED
     reportAltName AT ROW 8.86 COL 8 COLON-ALIGNED HELP
          "Enter Report Name"
     copyFormat AT ROW 8.86 COL 143 COLON-ALIGNED
     layoutFields AT ROW 8.91 COL 61 COLON-ALIGNED
     btnTestLayout AT ROW 8.91 COL 111 HELP
          "Click to Produce Test Layout Preview"
     btnSaveLayouts AT ROW 8.91 COL 116 HELP
          "Click to Save Layout"
     btnResetLayout AT ROW 8.91 COL 121 HELP
          "Reset Report Layout"
     btnDelete AT ROW 8.91 COL 126 HELP
          "Delete Report Layout"
     btnCopy AT ROW 8.91 COL 152 HELP
          "Copy from Selected Format"
     excludeLayout AT ROW 9 COL 99
     linePos AT ROW 9.1 COL 84 COLON-ALIGNED
     columnPos AT ROW 9.1 COL 92 COLON-ALIGNED
     "Locked Column" VIEW-AS TEXT
          SIZE 15 BY .71 AT ROW 1.48 COL 85
          BGCOLOR 12 
     "Non-Sortable Column" VIEW-AS TEXT
          SIZE 20 BY .71 AT ROW 1.48 COL 120
          BGCOLOR 15 
     "Hidden Column" VIEW-AS TEXT
          SIZE 15 BY .71 AT ROW 1.48 COL 142
          BGCOLOR 7 
     "Sortable Column" VIEW-AS TEXT
          SIZE 16 BY .71 AT ROW 1.48 COL 102
          BGCOLOR 14 
     RECT-7 AT ROW 1.24 COL 84
     RECT-8 AT ROW 1.24 COL 101
     RECT-9 AT ROW 1.24 COL 119
     RECT-10 AT ROW 1.24 COL 141
     SPACE(0.00) SKIP(26.71)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 
         TITLE "Browser Column Order and Report Layout".

DEFINE FRAME layoutFrame
     ruler-1 AT ROW 1 COL 1 NO-LABEL
     browserFilterFields AT ROW 13.86 COL 93
     btnSaveFilterFields AT ROW 13.86 COL 145 HELP
          "Click to Save Filter Fields"
     btnResetFilterFields AT ROW 13.86 COL 150 HELP
          "Reset Filter Fields from Last Save"
     btnTestExcel AT ROW 14.1 COL 39 HELP
          "Click to Produce Test Excel Preview"
     unAssigned AT ROW 14.81 COL 3 NO-LABEL
     excelOrder AT ROW 14.81 COL 49 NO-LABEL
     btnUp AT ROW 16.48 COL 41 HELP
          "Click to Move Current Column and Row Up"
     btnDown AT ROW 17.67 COL 41 HELP
          "Click to Move Current Column and Row Down"
     ruler-2 AT ROW 1.71 COL 1 NO-LABEL
     ruler-3 AT ROW 2.43 COL 1 NO-LABEL
     "3" VIEW-AS TEXT
          SIZE 2 BY .62 AT ROW 6.48 COL 1
          BGCOLOR 0 FGCOLOR 15 FONT 2
     "6" VIEW-AS TEXT
          SIZE 2 BY .62 AT ROW 10.76 COL 1
          BGCOLOR 0 FGCOLOR 15 FONT 2
     "1" VIEW-AS TEXT
          SIZE 2 BY .62 AT ROW 3.62 COL 1
          BGCOLOR 0 FGCOLOR 15 FONT 2
     "2" VIEW-AS TEXT
          SIZE 2 BY .62 AT ROW 5.05 COL 1
          BGCOLOR 0 FGCOLOR 15 FONT 2
     "Available Fields" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 14.1 COL 13
          BGCOLOR 14 
     "Excel Column Order" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 14.1 COL 56
          BGCOLOR 14 
     "4" VIEW-AS TEXT
          SIZE 2 BY .62 AT ROW 7.91 COL 1
          BGCOLOR 0 FGCOLOR 15 FONT 2
     "Unused Layout Fields:" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 12.19 COL 2
     "5" VIEW-AS TEXT
          SIZE 2 BY .62 AT ROW 9.33 COL 1
          BGCOLOR 0 FGCOLOR 15 FONT 2
     line-1 AT ROW 3.14 COL 1
     line-2 AT ROW 4.57 COL 1
     line-3 AT ROW 6 COL 1
     line-4 AT ROW 7.43 COL 1
     line-5 AT ROW 8.86 COL 1
     line-6 AT ROW 10.29 COL 1
     availableFields AT ROW 11.71 COL 1
     excelRect AT ROW 13.86 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS TOP-ONLY THREE-D 
         AT COL 2 ROW 10.05
         SIZE 155 BY 19.05
         BGCOLOR 15 
         TITLE "Job by Resource Report Layout".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* REPARENT FRAME */
ASSIGN FRAME layoutFrame:FRAME = FRAME Dialog-Frame:HANDLE.

/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB browseJob selectedColumn Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       browseJob:NUM-LOCKED-COLUMNS IN FRAME Dialog-Frame     = 5
       browseJob:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

ASSIGN 
       btnMove:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                " Move".

/* SETTINGS FOR BUTTON btnTestLayout IN FRAME Dialog-Frame
   1 2                                                                  */
/* SETTINGS FOR FILL-IN columnPos IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN linePos IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-10 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-7 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-8 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-9 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN selectedColumn IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME layoutFrame
   UNDERLINE                                                            */
/* BROWSE-TAB browserFilterFields excelRect layoutFrame */
/* SETTINGS FOR RECTANGLE availableFields IN FRAME layoutFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE excelRect IN FRAME layoutFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE line-1 IN FRAME layoutFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE line-2 IN FRAME layoutFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE line-3 IN FRAME layoutFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE line-4 IN FRAME layoutFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE line-5 IN FRAME layoutFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE line-6 IN FRAME layoutFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ruler-1 IN FRAME layoutFrame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN ruler-2 IN FRAME layoutFrame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN ruler-3 IN FRAME layoutFrame
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browseJob
/* Query rebuild information for BROWSE browseJob
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttblJob NO-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE browseJob */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browserFilterFields
/* Query rebuild information for BROWSE browserFilterFields
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH browseRptFields
  WHERE browseRptFields.rptID NE ''
    AND ID BEGINS browseRptFields.rptID.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE browserFilterFields */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME layoutFrame
/* Query rebuild information for FRAME layoutFrame
     _Query            is NOT OPENED
*/  /* FRAME layoutFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Browser Column Order and Report Layout */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME browseJob
&Scoped-define SELF-NAME browseJob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseJob Dialog-Frame
ON ROW-DISPLAY OF browseJob IN FRAME Dialog-Frame /* Browser Columns */
DO:
  {{&viewers}/includes/rowDisplay.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseJob Dialog-Frame
ON START-SEARCH OF browseJob IN FRAME Dialog-Frame /* Browser Columns */
DO:
  ASSIGN
    currentColumn = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN
    selectedColumn:SCREEN-VALUE = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN:LABEL + ' (' +
         (IF BROWSE {&BROWSE-NAME}:CURRENT-COLUMN:NAME EQ ? THEN 'calcTimeField'
          ELSE BROWSE {&BROWSE-NAME}:CURRENT-COLUMN:NAME) + ')'
    .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCopy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopy Dialog-Frame
ON CHOOSE OF btnCopy IN FRAME Dialog-Frame
DO:
  RUN copyLayout.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopy Dialog-Frame
ON RIGHT-MOUSE-CLICK OF btnCopy IN FRAME Dialog-Frame
DO:
  RUN {&loads}/columnsConvert.p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete Dialog-Frame
ON CHOOSE OF btnDelete IN FRAME Dialog-Frame
DO:
  RUN deleteLayout.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME layoutFrame
&Scoped-define SELF-NAME btnDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDown Dialog-Frame
ON CHOOSE OF btnDown IN FRAME layoutFrame /* DN */
DO:
  IF reportFormat EQ '' THEN RETURN NO-APPLY.
  RUN moveExcel (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define SELF-NAME btnHidden
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHidden Dialog-Frame
ON CHOOSE OF btnHidden IN FRAME Dialog-Frame /* Hide/UnHide */
DO:
  RUN setHiddenValue.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMove Dialog-Frame
ON CHOOSE OF btnMove IN FRAME Dialog-Frame /* Start Move */
DO:
  ASSIGN
    selectedColumn:SCREEN-VALUE = ''
    moveColumns = NOT moveColumns
    BROWSE {&BROWSE-NAME}:COLUMN-MOVABLE = moveColumns
    SELF:LABEL = '~&' + TRIM(STRING(moveColumns,'Stop/Start'))
               + SELF:PRIVATE-DATA
    .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnResetColumns
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnResetColumns Dialog-Frame
ON CHOOSE OF btnResetColumns IN FRAME Dialog-Frame
DO:
  opRunAgain = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME layoutFrame
&Scoped-define SELF-NAME btnResetFilterFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnResetFilterFields Dialog-Frame
ON CHOOSE OF btnResetFilterFields IN FRAME layoutFrame
DO:
  opRunAgain = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define SELF-NAME btnResetLayout
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnResetLayout Dialog-Frame
ON CHOOSE OF btnResetLayout IN FRAME Dialog-Frame
DO:
  opRunAgain = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSaveColumns
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSaveColumns Dialog-Frame
ON CHOOSE OF btnSaveColumns IN FRAME Dialog-Frame
DO:
  RUN saveColumns.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME layoutFrame
&Scoped-define SELF-NAME btnSaveFilterFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSaveFilterFields Dialog-Frame
ON CHOOSE OF btnSaveFilterFields IN FRAME layoutFrame
DO:
  RUN saveRptFields.
  MESSAGE 'Filter Fields Saved!' VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define SELF-NAME btnSaveLayouts
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSaveLayouts Dialog-Frame
ON CHOOSE OF btnSaveLayouts IN FRAME Dialog-Frame
DO:
  RUN saveRptLayout.
  RUN saveRptFormat.
  IF layoutFormat EQ '' THEN DO:
    FIND FIRST rptFormat
         WHERE rptFormat.rptName EQ reportName
           AND rptFormat.rptFormat EQ ''
         NO-ERROR.
    IF AVAILABLE rptFormat AND rptFormat.exclude NE excludeFormat THEN DO:
      rptFormat.exclude = excludeFormat.
      OUTPUT TO VALUE(staticDat + '{&data}/rptFormat.dat').
      FOR EACH rptFormat
          WHERE rptFormat.rptFormat EQ ''
            BY rptFormat.rptID BY rptFormat.rptName
          :
        EXPORT rptFormat.
      END. /* each rptFormat */
      OUTPUT CLOSE.
      FIND CURRENT rptFormat NO-LOCK.
      OS-COPY VALUE(staticDat + '{&data}/rptFormat.dat')
              VALUE(staticDat + '{&data}/rptFormat.sav').
    END. /* if avail */
  END. /* layoutformat blank */
  MESSAGE 'Report Layout Saved!' VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME layoutFrame
&Scoped-define SELF-NAME btnTestExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTestExcel Dialog-Frame
ON CHOOSE OF btnTestExcel IN FRAME layoutFrame
DO:
  RUN testPrint (YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTestExcel Dialog-Frame
ON RIGHT-MOUSE-CLICK OF btnTestExcel IN FRAME layoutFrame
DO:
  RUN {&viewers}/columns.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define SELF-NAME btnTestLayout
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTestLayout Dialog-Frame
ON CHOOSE OF btnTestLayout IN FRAME Dialog-Frame
DO:
  RUN testPrint (NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME layoutFrame
&Scoped-define SELF-NAME btnUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUp Dialog-Frame
ON CHOOSE OF btnUp IN FRAME layoutFrame /* UP */
DO:
  IF reportFormat EQ '' THEN RETURN NO-APPLY.
  RUN moveExcel (-1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define SELF-NAME copyFormat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL copyFormat Dialog-Frame
ON VALUE-CHANGED OF copyFormat IN FRAME Dialog-Frame /* Copy Format */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME layoutFrame
&Scoped-define SELF-NAME excelOrder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL excelOrder Dialog-Frame
ON DEFAULT-ACTION OF excelOrder IN FRAME layoutFrame
DO:
  IF reportFormat EQ '' THEN RETURN NO-APPLY.
  FIND FIRST rptLayout
       WHERE ROWID(rptLayout) EQ TO-ROWID(SELF:SCREEN-VALUE).
  rptLayout.excelColumn = 0.
  RUN excelBuild.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define SELF-NAME excludeLayout
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL excludeLayout Dialog-Frame
ON VALUE-CHANGED OF excludeLayout IN FRAME Dialog-Frame /* Exclude */
DO:
  ASSIGN {&SELF-NAME}
    excludeFormat = {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME layoutFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL layoutFields Dialog-Frame
ON VALUE-CHANGED OF layoutFields IN FRAME Dialog-Frame /* Fields */
DO:
  DEFINE VARIABLE fieldHandle AS HANDLE NO-UNDO.

  ASSIGN {&SELF-NAME}
    fieldHandle = WIDGET-HANDLE({&SELF-NAME})
    fieldHandle:SELECTED = YES
    .
  fieldHandle:MOVE-TO-TOP().
  APPLY 'SELECTION':U TO fieldHandle.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME reportAltName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL reportAltName Dialog-Frame
ON LEAVE OF reportAltName IN FRAME Dialog-Frame /* Name */
DO:
  ASSIGN {&SELF-NAME}
    alternateName = {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME reportFormat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL reportFormat Dialog-Frame
ON VALUE-CHANGED OF reportFormat IN FRAME Dialog-Frame /* Format */
DO:
  ASSIGN {&SELF-NAME}
    layoutFormat = {&SELF-NAME}.
  RUN createFields.
  RUN excelBuild.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME reportName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL reportName Dialog-Frame
ON VALUE-CHANGED OF reportName IN FRAME Dialog-Frame /* Report */
DO:
  ASSIGN {&SELF-NAME}
    selectedReport = {&SELF-NAME}.
  RUN createFields.
  RUN excelBuild.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME layoutFrame
&Scoped-define SELF-NAME unAssigned
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL unAssigned Dialog-Frame
ON DEFAULT-ACTION OF unAssigned IN FRAME layoutFrame
DO:
  IF reportFormat EQ '' THEN RETURN NO-APPLY.
  FIND rptFields NO-LOCK WHERE ROWID(rptFields) EQ TO-ROWID(SELF:SCREEN-VALUE).
  RUN findRptLayout (rptFields.fieldLabel,rptFields.fieldName).
  rptLayout.excelCol = excelOrder:NUM-ITEMS + 1.
  RUN excelBuild.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Dialog-Frame
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{{&viewers}/includes/viewersInclude.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN getConfiguration.
  RUN setView.
  RUN getCellColumns.
  RUN getReportLayout.
  RUN createFields.
  RUN enable_UI.
  RUN excelBuild.
  APPLY 'VALUE-CHANGED':U TO reportFormat.
  APPLY 'ENTRY':U TO reportName.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyLayout Dialog-Frame 
PROCEDURE copyLayout :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER bRptLayout FOR rptLayout.

  IF reportFormat EQ '' THEN DO:
    MESSAGE 'Default Format: Can not be Altered!' VIEW-AS ALERT-BOX.
    RETURN.
  END.
  IF reportFormat EQ copyFormat THEN DO:
    MESSAGE 'Formats selected are identical!' VIEW-AS ALERT-BOX.
    RETURN.
  END.
  RUN doDelete.
  FOR EACH bRptLayout
      WHERE bRptLayout.rptName EQ reportName
        AND bRptLayout.rptFormat EQ copyFormat
      :
    CREATE rptLayout.
    BUFFER-COPY bRptLayout EXCEPT bRptLayout.rptFormat TO rptLayout
      ASSIGN
        rptLayout.rptID = ID
        rptLayout.rptFormat = reportFormat
        .
  END. /* each brptlayout */
  RUN createFields.
  RUN excelBuild.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createFields Dialog-Frame 
PROCEDURE createFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lvRptLine AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvRptColumn AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvRowID AS ROWID NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  RUN LockWindowUpdate (FRAME {&FRAME-NAME}:HWND,OUTPUT i).
  DELETE WIDGET-POOL 'fieldPool' NO-ERROR.
  CREATE WIDGET-POOL 'fieldPool' PERSISTENT.
  layoutFields:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = ?.
  FOR EACH rptFields NO-LOCK:
    RUN findRptLayout (rptFields.fieldLabel,rptFields.fieldName).
    RUN createObject (rptFields.fieldLabel,rptFields.fieldName,rptFields.fieldFormat,
                      rptLayout.rptLine,rptLayout.rptColumn,ROWID(rptLayout)).
  END.
  RUN LockWindowUpdate (0,OUTPUT i).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createObject Dialog-Frame 
PROCEDURE createObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipLabel AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipName AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipFormat AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipLine AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipColumn AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.

  DEFINE VARIABLE idx AS INTEGER NO-UNDO.
  DEFINE VARIABLE pWidget AS HANDLE NO-UNDO.
  DEFINE VARIABLE pWidth AS INTEGER NO-UNDO.
  DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
  
  IF ipLabel BEGINS 'Status-' AND reportFormat NE '' THEN
  ASSIGN
    idx = INTEGER(SUBSTR(rptLayout.fieldLabel,8))
    ipLabel = IF customLabel[idx] NE '' THEN customLabel[idx] ELSE ipLabel.
  
  ASSIGN
    pWidth = MAX(LENGTH(ipFormat),LENGTH(ipLabel)) * 7
    ipLine = IF ipLine EQ 0 OR ipColumn EQ 0 THEN 230
             ELSE lineCoord[ipLine] + 1
    ipColumn = IF ipLine EQ 0 OR ipColumn EQ 0 THEN 115
               ELSE (ipColumn - 1) * 7.
  CREATE EDITOR pWidget IN WIDGET-POOL 'fieldPool'
    ASSIGN
      FRAME = FRAME layoutFrame:HANDLE
      FONT = 2
      X = ipColumn
      Y = ipLine
      READ-ONLY = YES
      BOX = NO
      WORD-WRAP = NO
      HIDDEN = NO
      SENSITIVE = YES
      MOVABLE = reportFormat NE ''
      SELECTABLE = YES
      BGCOLOR = 8
      SCREEN-VALUE = ipLabel + CHR(10) + ipFormat
      HEIGHT-PIXELS = 28
      WIDTH-PIXELS = pWidth
      PRIVATE-DATA = STRING(ipRowID)
    TRIGGERS:
      ON END-MOVE
         PERSISTENT RUN showCoord IN THIS-PROCEDURE (pWidget:HANDLE).
      ON SELECTION
         PERSISTENT RUN showCoord IN THIS-PROCEDURE (pWidget:HANDLE).
      ON DESELECTION
         PERSISTENT RUN deselectField IN THIS-PROCEDURE (pWidget:HANDLE).
    END TRIGGERS.
    pWidget:MOVE-TO-TOP().
    layoutFields:ADD-LAST(ipLabel,STRING(pWidget:HANDLE)) IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteLayout Dialog-Frame 
PROCEDURE deleteLayout :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF reportFormat EQ '' THEN DO:
    MESSAGE 'Default Format: Can not be Deleted!' VIEW-AS ALERT-BOX.
    RETURN.
  END. /* if reportformat */
  MESSAGE 'Delete this Layout?' VIEW-AS ALERT-BOX
    QUESTION BUTTONS YES-NO UPDATE deleteLayout AS LOGICAL.
  IF NOT deleteLayout THEN RETURN.
  RUN doDelete.
  ASSIGN
    reportFormat = ' '
    reportFormat:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ' '
    .
  RUN createFields.
  RUN excelBuild.
  MESSAGE 'Preform Save Layout for Delete Layout to be Complete!' VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deselectField Dialog-Frame 
PROCEDURE deselectField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipField AS HANDLE NO-UNDO.

  ipField:BGCOLOR = 8.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
  HIDE FRAME layoutFrame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doDelete Dialog-Frame 
PROCEDURE doDelete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH rptFormat
      WHERE rptFormat.rptName EQ reportName
        AND rptFormat.rptFormat EQ reportFormat
      :
    DELETE rptFormat.
  END. /* each rptformat */
  FOR EACH rptLayout
      WHERE rptLayout.rptName EQ reportName
        AND rptLayout.rptFormat EQ reportFormat
      :
    DELETE rptLayout.
  END. /* each rptLayout */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY selectedColumn reportName reportFormat reportAltName copyFormat 
          layoutFields excludeLayout linePos columnPos 
      WITH FRAME Dialog-Frame.
  ENABLE btnMove btnHidden btnSaveColumns btnResetColumns browseJob reportName 
         reportFormat reportAltName copyFormat layoutFields btnTestLayout 
         btnSaveLayouts btnResetLayout btnDelete btnCopy excludeLayout 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
  DISPLAY ruler-1 unAssigned excelOrder ruler-2 ruler-3 
      WITH FRAME layoutFrame.
  ENABLE browserFilterFields btnSaveFilterFields btnResetFilterFields 
         btnTestExcel unAssigned excelOrder btnUp btnDown 
      WITH FRAME layoutFrame.
  {&OPEN-BROWSERS-IN-QUERY-layoutFrame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excelBuild Dialog-Frame 
PROCEDURE excelBuild :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  ASSIGN
    unAssigned:LIST-ITEM-PAIRS IN FRAME layoutFrame = ?
    excelOrder:LIST-ITEM-PAIRS = ?.
  FOR EACH rptLayout NO-LOCK
      WHERE rptLayout.rptName EQ reportName
        AND rptLayout.rptFormat EQ reportFormat
        AND rptLayout.excelColumn NE 0
      BY rptLayout.excelColumn WITH FRAME layoutFrame
      :
    ASSIGN
      i = i + 1.
      rptLayout.excelColumn = i
      .
    excelOrder:ADD-LAST(STRING(rptLayout.excelColumn,'z9') + '. ' +
                               rptLayout.fieldLabel + ' (' +
                               rptLayout.fieldName + ')',STRING(ROWID(rptLayout))).
  END. /* each rptlayout */
  FOR EACH rptFields NO-LOCK WITH FRAME layoutFrame:
    IF NOT CAN-FIND(FIRST rptLayout
                    WHERE rptLayout.rptName EQ reportName
                      AND rptLayout.rptFormat EQ reportFormat
                      AND rptLayout.fieldLabel EQ rptFields.fieldLabel
                      AND rptLayout.fieldName EQ rptFields.fieldName
                      AND rptLayout.excelColumn NE 0) THEN
    unAssigned:ADD-LAST(rptFields.fieldLabel + ' (' +
                        rptFields.fieldName + ')',STRING(ROWID(rptFields))).
  END. /* each rptfields */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE findRptLayout Dialog-Frame 
PROCEDURE findRptLayout :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipFieldLabel AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipFieldName AS CHARACTER NO-UNDO.

  FIND FIRST rptLayout
       WHERE rptLayout.rptName EQ reportName
         AND rptLayout.rptFormat EQ reportFormat
         AND rptLayout.fieldLabel EQ ipFieldLabel
         AND rptLayout.fieldName EQ ipFieldName
       NO-ERROR.
  IF NOT AVAILABLE rptLayout THEN
  DO:
    CREATE rptLayout.
    ASSIGN
      rptLayout.rptID = ID
      rptLayout.rptName = reportName
      rptLayout.rptFormat = reportFormat
      rptLayout.fieldLabel = ipFieldLabel
      rptLayout.fieldName = ipFieldName
      .
  END. /* if not avail */
  FIND FIRST rptFormat
       WHERE rptFormat.rptName EQ reportName
         AND rptFormat.rptFormat EQ reportFormat
       NO-ERROR.
  ASSIGN
    excludeLayout:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(AVAILABLE rptFormat AND rptFormat.exclude)
    reportAltName:SCREEN-VALUE = IF AVAILABLE rptFormat THEN rptFormat.rptAltName ELSE ''
    alternateName = reportAltName:SCREEN-VALUE
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getReportLayout Dialog-Frame 
PROCEDURE getReportLayout :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  reportName:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = ?.
  FOR EACH rptNames
      WHERE rptNames.exclude EQ NO
      :
    reportName:ADD-LAST(rptNames.rptTitle,rptNames.rptName).
  END.
  reportName:SCREEN-VALUE = 'jobByResource'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveExcel Dialog-Frame 
PROCEDURE moveExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipMove AS INTEGER NO-UNDO.

  DEFINE VARIABLE saveRowID AS ROWID NO-UNDO.
  DEFINE VARIABLE currentCol AS INTEGER NO-UNDO.
  DEFINE VARIABLE newCol AS INTEGER NO-UNDO.

  DEFINE BUFFER bRptLayout FOR rptLayout.

  IF excelOrder:SCREEN-VALUE IN FRAME layoutFrame EQ ? THEN RETURN.
  FIND rptLayout NO-LOCK WHERE ROWID(rptLayout) EQ TO-ROWID(excelOrder:SCREEN-VALUE).
  IF ipMove EQ 1 AND rptLayout.excelColumn EQ excelOrder:NUM-ITEMS OR
     ipMove EQ -1 AND rptLayout.excelColumn EQ 1 THEN RETURN.
  ASSIGN
    saveRowID = ROWID(rptLayout)
    currentCol = rptLayout.excelColumn
    newCol = rptLayout.excelColumn + ipMove
    .
  FIND FIRST bRptLayout
       WHERE bRptLayout.rptName EQ reportName
         AND bRptLayout.rptFormat EQ reportFormat
         AND bRptLayout.excelColumn EQ newCol
       NO-ERROR.
  IF NOT AVAILABLE bRptLayout THEN RETURN.
  ASSIGN
    bRptLayout.excelColumn = currentCol
    rptLayout.excelColumn = newCol
    .
  RUN excelBuild.
  excelOrder:SCREEN-VALUE = STRING(saveRowID).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveColumns Dialog-Frame 
PROCEDURE saveColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cellColumn AS HANDLE NO-UNDO.
  DEFINE VARIABLE cellName AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  EMPTY TEMP-TABLE colOrder.
  FIND FIRST browseColumn WHERE browseColumn.colLabel EQ 'Status'
                            AND browseColumn.colName EQ 'status' NO-ERROR.
  CREATE colOrder.
  ASSIGN
    colOrder.colOrder = 9999
    colOrder.colHidden = YES
    colOrder.browseColumnRowID = ROWID(browseColumn)
    .
  DO i = 1 TO BROWSE {&BROWSE-NAME}:NUM-COLUMNS:
    ASSIGN
      cellColumn = BROWSE {&BROWSE-NAME}:GET-BROWSE-COLUMN(i)
      cellName = IF cellColumn:NAME NE ? THEN cellColumn:NAME ELSE 'calcTimeField'
      .
    FIND FIRST browseColumn WHERE browseColumn.colLabel EQ cellColumn:LABEL
                              AND browseColumn.colName EQ cellName NO-ERROR.
    IF NOT AVAILABLE browseColumn THEN NEXT.
    CREATE colOrder.
    ASSIGN
      colOrder.colOrder = cellColumn:X
      colOrder.colLocked = browseColumn.colLocked
      colOrder.colHidden = cellColumn:LABEL-BGCOLOR EQ 7
      colOrder.browseColumnRowID = ROWID(browseColumn)
      .
  END. /* do i */
  OUTPUT TO VALUE(SEARCH('{&data}/' + ID + '/columns.dat')).
  i = 0.
  FOR EACH browseColumn NO-LOCK WHERE browseColumn.colLocked EQ YES BY browseColumn.colOrder:
    i = browseColumn.colOrder.
    EXPORT browseColumn.
  END. /* browsecolumn */
  FOR EACH colOrder NO-LOCK WHERE colOrder.colLocked EQ NO AND colOrder.colHidden EQ NO:
    FIND FIRST browseColumn
         WHERE ROWID(browseColumn) EQ colOrder.browseColumnRowID
         NO-ERROR.
    IF NOT AVAILABLE browseColumn THEN NEXT.
    ASSIGN
      i = i + 1
      browseColumn.colOrder = i
      browseColumn.colHidden = NO
      .
    EXPORT browseColumn.
  END. /* each colorder */
  FOR EACH colOrder NO-LOCK
      WHERE colOrder.colHidden EQ YES
      :
    FIND browseColumn EXCLUSIVE-LOCK
         WHERE ROWID(browseColumn) EQ colOrder.browseColumnRowID NO-ERROR.
    IF NOT AVAILABLE browseColumn THEN NEXT.
    ASSIGN
      i = i + 1
      browseColumn.colOrder = i
      browseColumn.colHidden = YES
      .
    EXPORT browseColumn.
  END. /* each colorder */
  OUTPUT CLOSE.
  opRunAgain = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setHiddenValue Dialog-Frame 
PROCEDURE setHiddenValue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cellColumn AS HANDLE NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF selectedColumn:SCREEN-VALUE EQ '' THEN RETURN.
    IF currentColumn:LABEL-BGCOLOR EQ 12 THEN RETURN.
    currentColumn:LABEL-BGCOLOR = IF currentColumn:LABEL-BGCOLOR NE 7 THEN 7
                                  ELSE INTEGER(currentColumn:PRIVATE-DATA).
  END. /* do frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setView Dialog-Frame 
PROCEDURE setView :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF ipWidth LT FRAME {&FRAME-NAME}:WIDTH-PIXELS THEN
  ipWidth = FRAME {&FRAME-NAME}:WIDTH-PIXELS.
  IF ipHeight LT FRAME {&FRAME-NAME}:HEIGHT-PIXELS THEN
  ipHeight = FRAME {&FRAME-NAME}:HEIGHT-PIXELS.
  ASSIGN
    FRAME {&FRAME-NAME}:HEIGHT-PIXELS = ipHeight + 30
    FRAME {&FRAME-NAME}:WIDTH-PIXELS = ipWidth
    BROWSE {&BROWSE-NAME}:WIDTH-PIXELS = ipWidth - 15
    FRAME layoutFrame:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS
    FRAME layoutFrame:WIDTH-PIXELS = ipWidth - 15
    FRAME layoutFrame:VIRTUAL-HEIGHT-PIXELS = 680
    FRAME layoutFrame:VIRTUAL-WIDTH-PIXELS = 1690
    ruler-1:WIDTH-PIXELS IN FRAME layoutFrame = 1680
    ruler-2:WIDTH-PIXELS = 1680
    ruler-3:WIDTH-PIXELS = 1680
    line-1:WIDTH-PIXELS = 1680
    line-2:WIDTH-PIXELS = 1680
    line-3:WIDTH-PIXELS = 1680
    line-4:WIDTH-PIXELS = 1680
    line-5:WIDTH-PIXELS = 1680
    line-6:WIDTH-PIXELS = 1680
    availableFields:WIDTH-PIXELS = 1680
    lineCoord[1] = line-1:Y
    lineCoord[2] = line-2:Y
    lineCoord[3] = line-3:Y
    lineCoord[4] = line-4:Y
    lineCoord[5] = line-5:Y
    lineCoord[6] = line-6:Y
    lineCoord[7] = line-6:Y + line-6:HEIGHT-PIXELS + 1
    excelRect:HEIGHT-PIXELS = FRAME layoutFrame:HEIGHT-PIXELS - 295
    unAssigned:HEIGHT-PIXELS = excelRect:HEIGHT-PIXELS - 25
    excelOrder:HEIGHT-PIXELS = excelRect:HEIGHT-PIXELS - 25
    BROWSE browserFilterFields:HEIGHT-PIXELS = excelRect:HEIGHT-PIXELS
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showCoord Dialog-Frame 
PROCEDURE showCoord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipField AS HANDLE NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE colPos AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    FIND rptLayout EXCLUSIVE-LOCK WHERE ROWID(rptLayout) EQ TO-ROWID(ipField:PRIVATE-DATA).
    ASSIGN
      rptLayout.rptLine = 0
      rptLayout.rptColumn = 0
      layoutFields:SCREEN-VALUE = STRING(ipField:HANDLE)
      linePos:SCREEN-VALUE = '0'
      columnPos:SCREEN-VALUE = '0'
      ipField:BGCOLOR = 14
      .
    ipField:MOVE-TO-TOP().
    IF ipField:Y LT lineCoord[1] THEN ipField:Y = lineCoord[1].
    IF ipField:Y LT lineCoord[7] THEN
    DO i = EXTENT(lineCoord) - 1 TO 1 BY -1:
      IF ipField:Y LT lineCoord[i] THEN NEXT.
      ASSIGN
        colPos = TRUNCATE(ipField:X / 7,0) + 1
        linePos:SCREEN-VALUE = STRING(i)
        columnPos:SCREEN-VALUE = STRING(colPos)
        ipField:Y = lineCoord[i] + 1
        ipField:X = (colPos - 1) * 7
        rptLayout.rptLine = i
        rptLayout.rptColumn = colPos
        .
      LEAVE.
    END. /* do i */
  END. /* do with */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE testPrint Dialog-Frame 
PROCEDURE testPrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipExcel AS LOGICAL NO-UNDO.

  DEFINE VARIABLE runPrint AS CHARACTER NO-UNDO.
  DEFINE VARIABLE rptTitle AS CHARACTER NO-UNDO.

  ASSIGN
    rptTitle = IF reportAltName NE '' THEN reportAltName ELSE ''
    runPrint = findProgram('{&print}/',ID,'/' + reportName + '.r')
    linesPerPageValue = 66
    .
  IF runPrint EQ ? THEN DO:
    MESSAGE 'Report:' reportName 'does not exist!' VIEW-AS ALERT-BOX.
    RETURN.
  END. /* if runprint */
  RUN VALUE(runPrint) ('{&Board}',reportFormat,rptTitle,ipExcel,NO /*showParametersPage*/).
  IF ipExcel THEN
  OS-COMMAND NO-WAIT start excel.exe VALUE(SEARCH(printFile)).
  ELSE
  OS-COMMAND NO-WAIT notepad.exe VALUE(printFile).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

