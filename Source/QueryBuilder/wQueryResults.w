&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wQuery
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wQuery 
/*------------------------------------------------------------------------
    File        : wDataQuery.w
    Purpose     : Actual databrowse for user query

    Author(s)   : Patrick Tingen
    Created     : 2019

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

{queryLib.i &reference-only=reference-only}

DEFINE INPUT  PARAMETER phParent AS HANDLE NO-UNDO.
DEFINE OUTPUT PARAMETER phFrame  AS HANDLE NO-UNDO.

CREATE WIDGET-POOL.

/* Detect bitness of running Progress version
 * See Progress kb #54631
 */
&IF PROVERSION <= '8' &THEN  /* OE 10+ */
  &IF PROVERSION >= '11.3' &THEN   /* PROCESS-ARCHITECTURE function is available */
    &IF PROCESS-ARCHITECTURE = 32 &THEN /* 32-bit pointers */
      &GLOBAL-DEFINE POINTERTYPE LONG
      &GLOBAL-DEFINE POINTERBYTES 4
    &ELSEIF PROCESS-ARCHITECTURE = 64 &THEN /* 64-bit pointers */
      &GLOBAL-DEFINE POINTERTYPE INT64
      &GLOBAL-DEFINE POINTERBYTES 8
    &ENDIF  /* PROCESS-ARCHITECTURE */
  &ELSE   /* Can't check architecture pre-11.3 so default to 32-bit */
    &GLOBAL-DEFINE POINTERTYPE LONG
    &GLOBAL-DEFINE POINTERBYTES 4
  &ENDIF  /* PROVERSION > 11.3 */
&ELSE   /* pre-OE10 always 32-bit on Windows */
  &GLOBAL-DEFINE POINTERTYPE LONG
  &GLOBAL-DEFINE POINTERBYTES 4
&ENDIF  /* PROVERSION < 8 */

DEFINE VARIABLE ghDataBrowse            AS HANDLE      NO-UNDO.
DEFINE VARIABLE giMaxColumns            AS INTEGER     NO-UNDO INITIAL 100.
DEFINE VARIABLE gcColumnHandles         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcColumnNames           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE giRowNr                 AS INTEGER     NO-UNDO.
DEFINE VARIABLE giWindowLockCounter     AS INTEGER     NO-UNDO.
DEFINE VARIABLE ghLastFilterField       AS HANDLE      NO-UNDO.
DEFINE VARIABLE giLightGray             AS INTEGER     NO-UNDO.
DEFINE VARIABLE gcPreviousValues        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ghDataTable             AS HANDLE      NO-UNDO.
DEFINE VARIABLE ghDataBuffer            AS HANDLE      NO-UNDO.
DEFINE VARIABLE gcSortColumn            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE glSortAscending         AS LOGICAL     NO-UNDO.
DEFINE VARIABLE gcBaseQuery             AS CHARACTER   NO-UNDO.

/* TT for sorting options in user query */
DEFINE TEMP-TABLE ttQuerySort NO-UNDO RCODE-INFORMATION
  FIELD iGroup     AS INTEGER /* 1:query, 2:browse */
  FIELD iSortNr    AS INTEGER
  FIELD cSortField AS CHARACTER
  FIELD lAscending AS LOGICAL
  FIELD iExt       AS INTEGER
  INDEX iPrim IS PRIMARY iGroup iSortNr
  .
 
PROCEDURE GetKeyboardState EXTERNAL "user32.dll":
  DEFINE INPUT  PARAMETER KBState AS {&POINTERTYPE}. /* memptr */
  DEFINE RETURN PARAMETER RetVal  AS LONG. /* bool   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frData

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnClearDataFilter 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD createMenu wQuery 
FUNCTION createMenu RETURNS HANDLE
  ( phParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD createMenuItem wQuery 
FUNCTION createMenuItem RETURNS HANDLE
  ( phMenu    AS HANDLE
  , pcType    AS CHARACTER
  , pcLabel   AS CHARACTER
  , pcName    AS CHARACTER
  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD filterModified wQuery 
FUNCTION filterModified RETURNS LOGICAL
  ( phFilterField AS HANDLE
  , plModified    AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKeyList wQuery 
FUNCTION getKeyList RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSafeFormat wQuery 
FUNCTION getSafeFormat RETURNS CHARACTER
  ( pcFormat   AS CHARACTER 
  , pcDataType AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD killMenu wQuery 
FUNCTION killMenu RETURNS LOGICAL
  ( phMenu AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD lockWindow wQuery 
FUNCTION lockWindow RETURNS LOGICAL
  ( plLockWindow AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFilterFieldColor wQuery 
FUNCTION setFilterFieldColor RETURNS LOGICAL
  ( phWidget AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wQuery AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClearDataFilter  NO-FOCUS FLAT-BUTTON
     LABEL "C" 
     SIZE-PIXELS 20 BY 21 TOOLTIP "clear all filters #(SHIFT-DEL)".

DEFINE RECTANGLE rctData
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE-PIXELS 1000 BY 298
     BGCOLOR 17 .

DEFINE RECTANGLE rctDataFilter
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE-PIXELS 1000 BY 28
     BGCOLOR 12 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frData
     btnClearDataFilter AT Y 5 X 980 WIDGET-ID 76
     rctData AT Y 0 X 5 WIDGET-ID 272
     rctDataFilter AT Y 1 X 5 WIDGET-ID 296
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 201 BY 14.52 WIDGET-ID 100.


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
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wQuery ASSIGN
         HIDDEN             = YES
         TITLE              = "Query results"
         HEIGHT             = 14.86
         WIDTH              = 203
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN wQuery = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wQuery
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME frData
   FRAME-NAME                                                           */
ASSIGN 
       btnClearDataFilter:HIDDEN IN FRAME frData           = TRUE.

/* SETTINGS FOR RECTANGLE rctData IN FRAME frData
   NO-ENABLE                                                            */
ASSIGN 
       rctData:HIDDEN IN FRAME frData           = TRUE.

/* SETTINGS FOR RECTANGLE rctDataFilter IN FRAME frData
   NO-ENABLE                                                            */
ASSIGN 
       rctDataFilter:HIDDEN IN FRAME frData           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME btnClearDataFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearDataFilter wQuery
ON CHOOSE OF btnClearDataFilter IN FRAME frData /* C */
DO:
  RUN btnClearDataFilterChoose.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wQuery 


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

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   
  RUN enable_UI.
  phFrame = FRAME {&FRAME-NAME}:HANDLE.
  RUN initObject. 
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnClearDataFilterChoose wQuery 
PROCEDURE btnClearDataFilterChoose :
/* Clear filters and reopen data browse
 */
  DEFINE BUFFER bField FOR ttField.

  FOR EACH bField:
    IF VALID-HANDLE(bField.hFilter) THEN
    DO:
      bField.hFilter:SCREEN-VALUE = bField.hFilter:PRIVATE-DATA.
      FilterModified(bField.hFilter,NO).
      setFilterFieldColor(bField.hFilter).
    END.
  END.

  RUN reopenQuery.

END PROCEDURE. /* btnClearDataFilterChoose */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildBrowse wQuery 
PROCEDURE buildBrowse :
/* Create the browse and open the query
 */
  DEFINE INPUT PARAMETER phQuery AS HANDLE NO-UNDO.
  
  DEFINE VARIABLE cMyFormat      AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE hMenu          AS HANDLE      NO-UNDO.
  DEFINE VARIABLE hMenuItem      AS HANDLE      NO-UNDO.
  DEFINE VARIABLE iColumn        AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iExtent        AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iColumnWidth   AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iMinWidth      AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cColumnName    AS CHARACTER   NO-UNDO.

  DEFINE BUFFER bField FOR ttField.
  DEFINE BUFFER bTable FOR ttTable.

  IF VALID-HANDLE(ghDataBrowse) THEN DELETE OBJECT ghDataBrowse NO-ERROR.
  rctDataFilter:VISIBLE IN FRAME frData = FALSE. 
  
  /* Start building */
  CREATE BROWSE ghDataBrowse
    ASSIGN
    NAME              = "brData"
    FRAME             = FRAME frData:HANDLE
    QUERY             = phQuery
    MULTIPLE          = TRUE
    X                 = rctData:X + 3
    Y                 = rctData:Y + 5 + 21 /* extra space for filters */
    WIDTH-PIXELS      = rctData:WIDTH-PIXELS - 10
    HEIGHT-PIXELS     = rctData:HEIGHT-PIXELS - 10 - 23 /* extra space for filters */
    ROW-MARKERS       = TRUE
    SEPARATORS        = TRUE
    SENSITIVE         = TRUE
    VISIBLE           = FALSE
    NO-VALIDATE       = TRUE
    COLUMN-RESIZABLE  = TRUE
    COLUMN-SCROLLING  = TRUE /* scroll with whole columns at a time */
    TRIGGERS:
      ON "CTRL-A"           PERSISTENT RUN dataSelect        IN THIS-PROCEDURE (ghDataBrowse, TRUE).
      ON "CTRL-D"           PERSISTENT RUN dataSelect        IN THIS-PROCEDURE (ghDataBrowse, FALSE).
      ON "CTRL-J"           PERSISTENT RUN openQuery         IN THIS-PROCEDURE.
      ON "ROW-DISPLAY"      PERSISTENT RUN dataRowDisplay    IN THIS-PROCEDURE.
      ON "START-SEARCH"     PERSISTENT RUN dataColumnSort    IN THIS-PROCEDURE.
      ON "SCROLL-NOTIFY"    PERSISTENT RUN dataScrollNotify  IN THIS-PROCEDURE (ghDataBrowse).
      ON "DEFAULT-ACTION"   PERSISTENT RUN dataDoubleClick   IN THIS-PROCEDURE.
      ON "CTRL-CURSOR-UP"   PERSISTENT RUN dataGotoFilter    IN THIS-PROCEDURE.
      ON "F5"               PERSISTENT RUN reopenQuery       IN THIS-PROCEDURE.
      ON "F12"              PERSISTENT RUN showQuery         IN THIS-PROCEDURE. 
    END TRIGGERS.

  /* Add the columns to the browse */
  gcColumnHandles = "".
  gcColumnNames = "".
  iColumn = 0.

  #addColumnLoop:
  FOR EACH bField 
    , EACH bTable WHERE bTable.tableName = bField.tableName
    BY bField.orderNr:

    /* Protect against too much columns. This gives error:
     * SYSTEM ERROR: stkpush: stack overflow. Increase the -s parameter. (279)
     */
    iColumn = iColumn + 1.
    IF iColumn > giMaxColumns THEN LEAVE #addColumnLoop.

    /* Get handle to this field in the buffer */
    cColumnName = ENTRY(NUM-ENTRIES(bField.fieldName,'.'), bField.fieldName, '.').
    IF NUM-ENTRIES(cColumnName,'[') > 1 THEN
      ASSIGN 
        iExtent     = INTEGER(ENTRY(1,ENTRY(NUM-ENTRIES(cColumnName,'['), cColumnName,'[') ,']'))
        cColumnName = ENTRY(1,cColumnName, '[').
    ELSE 
      ASSIGN 
        iExtent = 0.    
    
    bField.hField = phQuery:GET-BUFFER-HANDLE(bTable.orderNr):BUFFER-FIELD(cColumnName):HANDLE NO-ERROR.

/*     MESSAGE bField.orderNr cColumnName iExtent VALID-HANDLE(bField.hField) */
/*       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.                            */
    
    /* Get a format that protects against 'Value X could not be displayed using Y' */
    cMyFormat = getSafeFormat(cMyFormat, bField.dataType).

    /* Apply the format */
    IF NOT cMyFormat BEGINS "HH:MM" THEN
    DO:
      bField.hField:FORMAT = cMyFormat NO-ERROR.
      IF ERROR-STATUS:ERROR THEN bField.hField:FORMAT = bField.fieldFormat NO-ERROR. /* Restore original format */
    END.

    /* Add a calculated column for integers with time format */
    IF (   bField.dataType = "DECIMAL"
        OR bField.dataType BEGINS "INT") /* use BEGINS to cover integer / int64 and extents of both */
      AND bField.fieldFormat BEGINS "HH:MM" THEN
    DO:
      bField.hColumn = ghDataBrowse:ADD-CALC-COLUMN("character","x(8)","", bField.fieldName ) NO-ERROR.
    END.
    ELSE
    DO:
      bField.hColumn = ghDataBrowse:ADD-LIKE-COLUMN(bField.fieldName).
    END.

    /* Set label and name */
    bField.hColumn:LABEL     = bField.fieldLabel.
    bField.hColumn:NAME      = bField.fieldName.
    bField.hColumn:READ-ONLY = TRUE.
    bField.hColumn:VISIBLE   = TRUE.

    ON "end-resize" OF bField.hColumn PERSISTENT RUN dataScrollNotify IN THIS-PROCEDURE(ghDataBrowse).

    /* Build a list of column Handles for the rowDisplay trigger */
    ASSIGN
      gcColumnHandles = gcColumnHandles + "," + STRING(bField.hColumn)
      gcColumnNames   = gcColumnNames + "," + bField.fieldName
      .

    /* Create a filterfield for this column
     */
    CREATE COMBO-BOX bField.hFilter
      ASSIGN
        SUBTYPE       = "DROP-DOWN"
        FRAME         = ghDataBrowse:FRAME
        NAME          = "filter_" + bField.fieldName
        X             = ghDataBrowse:X
        Y             = rctData:Y + 5 + 21 - 23 /* Extra space for filters */
        WIDTH-PIXELS  = 10
        SENSITIVE     = TRUE
        VISIBLE       = TRUE
        FORMAT        = "x(40)"
        PRIVATE-DATA  = bField.fieldName
        SCREEN-VALUE  = ENTRY(1,cColumnName,'[')
        INNER-LINES   = 10
        DELIMITER     = CHR(1)
        MODIFIED      = NO /*170901*/
        .

    /* Add context menu to combo */
    hMenu = createMenu(bField.hFilter).
    bField.hFilter:POPUP-MENU = hMenu.

    /* Clear all filters */
    hMenuItem = createMenuItem(hMenu,"Item","Clear All &Filters","").
    ON "CHOOSE" OF hMenuItem PERSISTENT RUN applyEvent IN THIS-PROCEDURE (btnClearDataFilter:handle,"choose").

    /* Clear history */
    hMenuItem = createMenuItem(hMenu,"Item","Clear &History","").
    ON "CHOOSE" OF hMenuItem PERSISTENT RUN clearDataFilter IN THIS-PROCEDURE (bField.hFilter).

    /* Sort list */
    hMenuItem = createMenuItem(hMenu,"Item","&Sort List","").
    ON "CHOOSE" OF hMenuItem PERSISTENT RUN sortComboBox IN THIS-PROCEDURE (bField.hFilter).

    /* RULE / Cut / Copy / Paste / Delete */
    hMenuItem = createMenuItem(hMenu,"RULE","","").

    /* Cut */
    hMenuItem = createMenuItem(hMenu,"ITEM","Cut","").
    ON "CHOOSE" OF hMenuItem PERSISTENT RUN cutToClipboard IN THIS-PROCEDURE (bField.hFilter).

    /* Copy */
    hMenuItem = createMenuItem(hMenu,"ITEM","C&opy","").
    ON "CHOOSE" OF hMenuItem PERSISTENT RUN copyToClipboard IN THIS-PROCEDURE (bField.hFilter).

    /* Paste */
    hMenuItem = createMenuItem(hMenu,"ITEM","Paste","").
    ON "CHOOSE" OF hMenuItem PERSISTENT RUN pasteFromClipboard IN THIS-PROCEDURE (bField.hFilter).

    /* Delete */
    hMenuItem = createMenuItem(hMenu,"ITEM","Delete","").
    ON "CHOOSE" OF hMenuItem PERSISTENT RUN clearField IN THIS-PROCEDURE (bField.hFilter).

    /* triggers */
    ON "CTRL-A"           OF bField.hFilter PERSISTENT RUN dataSelect              IN THIS-PROCEDURE (ghDataBrowse, TRUE).
    ON "CTRL-D"           OF bField.hFilter PERSISTENT RUN dataSelect              IN THIS-PROCEDURE (ghDataBrowse, FALSE).
    ON "ENTRY"            OF bField.hFilter PERSISTENT RUN filterFieldEntry        IN THIS-PROCEDURE (bField.hFilter).
    ON "LEAVE"            OF bField.hFilter PERSISTENT RUN filterFieldLeave        IN THIS-PROCEDURE (bField.hFilter).
    ON "ANY-PRINTABLE"    OF bField.hFilter PERSISTENT RUN filterFieldAnyPrintable IN THIS-PROCEDURE (bField.hFilter).
    ON "VALUE-CHANGED"    OF bField.hFilter PERSISTENT RUN filterFieldAnyPrintable IN THIS-PROCEDURE (bField.hFilter).
    ON "SHIFT-DEL"        OF bField.hFilter PERSISTENT RUN filterFieldClearAll     IN THIS-PROCEDURE (bField.hFilter, btnClearDataFilter:HANDLE).
    ON "RETURN"           OF bField.hFilter PERSISTENT RUN reopenQuery             IN THIS-PROCEDURE.
    ON "F2"               OF bField.hFilter PERSISTENT RUN reopenQuery             IN THIS-PROCEDURE.
    ON "F5"               OF bField.hFilter PERSISTENT RUN reopenQuery             IN THIS-PROCEDURE.
    ON "CTRL-CURSOR-DOWN" OF bField.hFilter PERSISTENT RUN filterFieldCursorDown   IN THIS-PROCEDURE (bField.hFilter, bField.hColumn).

    /* Connect filter to field and set color */
    bField.hField:PRIVATE-DATA = STRING(bField.hFilter).

  END. /* #addColumnLoop */

  gcColumnHandles = TRIM(gcColumnHandles,",").
  gcColumnNames = TRIM(gcColumnNames,",").

  /* Create the context menu for the databrowse if that has not been done yet */
/*   RUN createMenuDataBrowse. */

  /* Show the browse */
  ghDataBrowse:VISIBLE = TRUE.
         
  /* Limit fields to a max of 300px wide
   * This must be done after the browse is realized
   */
  adjustFilterLoop:
  FOR EACH bField WHERE VALID-HANDLE(bField.hColumn):

    /* Make sure it is not wider than 300px */
    iColumnWidth = bField.hColumn:WIDTH-PIXELS.
    IF iColumnWidth = ? THEN iColumnWidth = MINIMUM(300, bField.hColumn:WIDTH-PIXELS).

    /* Make sure the column is at least as wide as its name */
    /* And if the filter is of type COMBO, reserve some extra space for the arrow down */
    iMinWidth = FONT-TABLE:GET-TEXT-WIDTH-PIXELS(bField.hColumn:NAME, bField.hColumn:FONT).
    IF iColumnWidth < iMinWidth THEN iColumnWidth = iMinWidth.

    bField.hColumn:WIDTH-PIXELS = iColumnWidth.
    bField.hFilter:WIDTH-PIXELS = iColumnWidth.
  END.

  /* Adjust all filters */
  RUN dataScrollNotify(ghDataBrowse).

/*   /* Reset the TAB order of the filter fields */ */
/*   RUN setFilterFieldTabOrder.                    */

END PROCEDURE. /* buildBrowse */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildQuery wQuery 
PROCEDURE buildQuery :
/* Build the query and return the handle
*/
  DEFINE OUTPUT PARAMETER phQuery AS HANDLE NO-UNDO.
  
  DEFINE VARIABLE hBuffer    AS HANDLE      NO-UNDO.
  DEFINE VARIABLE cQuery     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cAnd       AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE lQueryOk   AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cCondition AS CHARACTER   NO-UNDO.
  
  DEFINE BUFFER bTable FOR ttTable.
  DEFINE BUFFER bField FOR ttField.
  DEFINE BUFFER bQuery FOR ttQuery.
  
  DO WITH FRAME {&frame-name}:
    FIND bQuery.
    CREATE QUERY phQuery.
    
    FOR EACH bTable BY bTable.orderNr:
    
      CREATE BUFFER hBuffer FOR TABLE bTable.tableName.
      phQuery:ADD-BUFFER(hBuffer).
      
      /* Set buffer handle for all fields of this table */
      FOR EACH bField WHERE bField.tableName = bTable.tableName:
        bField.hBuffer = hBuffer.
      END.
      
      cAnd = ''.
      
      IF cQuery = '' THEN
        cQuery = SUBSTITUTE('FOR EACH &1 NO-LOCK', bTable.tableName).
      ELSE 
      DO:
        IF bTable.autoJoin THEN
          cQuery = SUBSTITUTE('&1, EACH &2 NO-LOCK OF &3', cQuery, bTable.tableName, bTable.parentTable).
        ELSE
          cQuery = SUBSTITUTE('&1, EACH &2 NO-LOCK', cQuery, bTable.tableName).      
      END.
        
      cCondition = TRIM(bTable.conditions,'~n ').
      IF cCondition <> '' THEN
      DO:
        cAnd = (IF cAnd = '' THEN 'WHERE' ELSE 'AND').
        cQuery = SUBSTITUTE('&1 &2 &3'
                           , cQuery
                           , (IF cCondition BEGINS cAnd THEN '' ELSE cAnd)
                           , cCondition).    
      END.           
    END.
    
    cCondition = TRIM(bQuery.conditions,'~n ').
    IF cCondition <> '' THEN
    DO:
      cAnd = (IF cAnd = '' THEN 'WHERE' ELSE 'AND').
      
      /* this makes it easier to add the user filtering */
      IF cCondition = '' AND cAnd = 'WHERE' THEN cCondition = 'WHERE TRUE'. 
      
      cQuery = SUBSTITUTE('&1 &2 &3'
                         , cQuery
                         , (IF cCondition BEGINS cAnd THEN '' ELSE cAnd)
                         , cCondition).
    END. 
    
    /* Strip strange combinations of operators */
    cQuery = REPLACE(cQuery,' WHERE AND ', ' WHERE ').
    
    /* Add INDEXED-REPOSITION */
    IF LOOKUP('INDEXED-REPOSITION', cQuery, ' ') = 0 THEN
      cQuery = SUBSTITUTE('&1 INDEXED-REPOSITION', cQuery).
    
    /* Try to open it */
    lQueryOk = phQuery:QUERY-PREPARE(cQuery) NO-ERROR. 
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataColumnSort wQuery 
PROCEDURE dataColumnSort :
/* Sort on a datacolumn
   */

  /* Reverse the sort direction if you click the same column again */ 
  IF gcSortColumn = SELF:CURRENT-COLUMN:NAME THEN
  DO:
    /* Toggle between ASCENDING / DESCENDING / NONE */
    IF glSortAscending = FALSE THEN
      ASSIGN 
        gcSortColumn    = ''
        glSortAscending = FALSE.
    ELSE         
      ASSIGN 
        glSortAscending = NOT glSortAscending.
  END.
  ELSE 
    ASSIGN 
      gcSortColumn    = SELF:CURRENT-COLUMN:NAME    
      glSortAscending = TRUE.

  RUN reopenQuery.

END PROCEDURE. /* dataColumnSort */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataGotoFilter wQuery 
PROCEDURE dataGotoFilter :
/* Jump from browse straight to the filter fields
 */
  DEFINE BUFFER bField FOR ttField.

  /* If we have been in the filters before, the name of the last visited
   * filter field is in ghLastFilterField. Try to jump back to that field.
   */
  FIND bField WHERE bField.hFilter = ghLastFilterField NO-ERROR.
  IF NOT AVAILABLE bField THEN FIND bField WHERE bField.orderNr = 1 NO-ERROR.  
  IF NOT AVAILABLE bField THEN FIND FIRST bField NO-ERROR.
  IF AVAILABLE bField THEN APPLY 'entry' TO bField.hFilter.

END PROCEDURE. /* dataGotoFilter */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataRowDisplay wQuery 
PROCEDURE dataRowDisplay :
/* Set the background color to another color to get an odd/even coloring of the rows.
 */

  DEFINE VARIABLE i       AS INTEGER     NO-UNDO.
  DEFINE VARIABLE hColumn AS HANDLE      NO-UNDO.
  
  giRowNr = giRowNr + 1.

  DO i = 1 TO NUM-ENTRIES(gcColumnHandles):
    hColumn = HANDLE(ENTRY(i,gcColumnHandles)).
    hColumn:BGCOLOR = (IF giRowNr MOD 2 = 0 THEN 15 ELSE giLightGray).    
  END.
 
END PROCEDURE. /* dataRowDisplay */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataScrollNotify wQuery 
PROCEDURE dataScrollNotify :
/* Adjust size and position of the filterfields to browse
 */
  DEFINE INPUT PARAMETER phBrowse AS HANDLE NO-UNDO.

  DEFINE VARIABLE cFilterFields AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cButtons      AS CHARACTER NO-UNDO.

  DEFINE BUFFER bField FOR ttField.

  /* Might get called when browse is not yet realized, so: */
  IF NOT VALID-HANDLE(phBrowse) THEN RETURN.

  /* Freeze all */
  lockWindow(YES).

  getFilterLoop:
  FOR EACH bField BY bField.orderNr:
    IF VALID-HANDLE(bField.hFilter) THEN
      cFilterFields = TRIM(SUBSTITUTE('&1,&2', cFilterFields, bField.hFilter),',').
  END.

  DO WITH FRAME frData:
    cButtons = SUBSTITUTE('&1', btnClearDataFilter:HANDLE).
  END.

  /* Resize them */
  RUN resizeFilterFields
    ( INPUT cFilterFields
    , INPUT cButtons
    , INPUT phBrowse
    ).

  /* If the first filter value happens to be the same as the shadow text,
   * progress will 'select' it, wich looks weird, so we need to normalize it
   */
  FOR EACH bField BY bField.orderNr:
    IF VALID-HANDLE(bField.hFilter) THEN RUN FilterFieldLeave(bField.hFilter).
  END.

  lockWindow(NO).
  RETURN NO-APPLY.

END PROCEDURE.  /* dataScrollNotify */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataSelect wQuery 
PROCEDURE dataSelect :
/* Select all records in the browse
  */
  DEFINE INPUT PARAMETER phBrowse AS HANDLE  NO-UNDO.
  DEFINE INPUT PARAMETER plSelect AS LOGICAL NO-UNDO.

  lockWindow(YES).
  SESSION:SET-WAIT-STATE('general').
  
  IF plSelect THEN
    phBrowse:SELECT-ALL().
  ELSE 
    phBrowse:DESELECT-ROWS().
  
  lockWindow(NO).
  SESSION:SET-WAIT-STATE('').

END PROCEDURE. /* dataSelect */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wQuery  _DEFAULT-DISABLE
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
  HIDE FRAME frData.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wQuery  _DEFAULT-ENABLE
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
  ENABLE btnClearDataFilter 
      WITH FRAME frData.
  {&OPEN-BROWSERS-IN-QUERY-frData}
  VIEW wQuery.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filterFieldAnyPrintable wQuery 
PROCEDURE filterFieldAnyPrintable :
/* Set modified flag if character is typed
 */
  DEFINE INPUT PARAMETER phFilterField AS HANDLE NO-UNDO.

  FilterModified(phFilterField,TRUE).

END PROCEDURE. /* filterFieldAnyPrintable */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filterFieldClearAll wQuery 
PROCEDURE filterFieldClearAll :
/* Wipe contents of all filter fields in the same group
 */
  DEFINE INPUT PARAMETER phFilterField AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER phClearButton AS HANDLE NO-UNDO.

  lockWindow(YES).

  APPLY "choose" TO phClearButton. /* clear them all */
  APPLY "entry"  TO phFilterField. /* set focus */

  lockWindow(NO).

END PROCEDURE. /* filterFieldClearAll */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filterFieldCursorDown wQuery 
PROCEDURE filterFieldCursorDown :
/* Jump from filter field to browse on cursor down
 */
  DEFINE INPUT PARAMETER phFilterField  AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER phBrowseField  AS HANDLE NO-UNDO.

  ghLastFilterField = phFilterField.
  APPLY 'leave' TO phFilterField.
  APPLY 'entry' TO phBrowseField.
  

END PROCEDURE. /* filterFieldCursorDown */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filterFieldEntry wQuery 
PROCEDURE filterFieldEntry :
/* Set the color for the text in the filter to black
 */
  DEFINE INPUT PARAMETER phFilterField AS HANDLE NO-UNDO.

  /* If you enter the field and you have not put in a filter,
   * clear out the field so you can type something yourself
   */
  IF FilterModified(phFilterField,?) = FALSE THEN
    phFilterField:SCREEN-VALUE = ''.

  setFilterFieldColor(phFilterField).

END PROCEDURE. /* filterFieldEntry */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filterFieldLeave wQuery 
PROCEDURE filterFieldLeave :
/* Set the color for the text in the filter to gray
 */
  DEFINE INPUT PARAMETER phFilterField      AS HANDLE  NO-UNDO.

  /* If nothing in the filter, restore the shadow text */
  IF FilterModified(phFilterField,?) = FALSE
    OR phFilterField:SCREEN-VALUE = ''
    OR phFilterField:SCREEN-VALUE = ? THEN
  DO:
    FilterModified(phFilterField,FALSE).
    phFilterField:SCREEN-VALUE = phFilterField:PRIVATE-DATA.
  END.

  setFilterFieldColor(phFilterField).

END PROCEDURE. /* filterFieldLeave */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getFilter wQuery 
PROCEDURE getFilter :
/* return a string with the user defined filtering
*/
  DEFINE OUTPUT PARAMETER pcFilter AS CHARACTER NO-UNDO.

  DEFINE VARIABLE cOperator  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cValue     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cValueList AS CHARACTER NO-UNDO.

  DEFINE BUFFER bField FOR ttField.

  /* Collect all filters */
  FOR EACH bField WHERE VALID-HANDLE(bField.hFilter):
    
    /* Skip fields with shadowtext or empty value */
    IF   bField.hFilter:SCREEN-VALUE = bField.fieldName
      OR bField.hFilter:SCREEN-VALUE = ""
      OR bField.hFilter:SCREEN-VALUE = ? THEN NEXT.

    ASSIGN cValue = bField.hFilter:SCREEN-VALUE.

    /* Add to combo */
    IF LOOKUP(cValue, bField.hFilter:LIST-ITEMS) = 0 THEN 
      bField.hFilter:ADD-LAST(cValue).
    
    bField.hFilter:SCREEN-VALUE = cValue.
    RUN filterFieldLeave(bField.hFilter). 
    
    cOperator = SUBSTRING(cValue, 1, 2).
    DO WHILE LOOKUP(cOperator, "=,<,>,<=,>=,<>,!,!=") = 0 AND LENGTH(cOperator) > 0:
      cOperator = SUBSTRING(cOperator, 1, LENGTH(cOperator) - 1).
    END.
    
    /* Don't trim spaces in value, use RIGHT-TRIM on value, not TRIM */
    ASSIGN 
      cValue    = IF cOperator <> "" THEN RIGHT-TRIM(SUBSTRING(cValue, LENGTH(cOperator) + 1)) ELSE cValue
      cValue    = TRIM(cValue,"'~"") /* Remove surrounding quotes */
      cOperator = REPLACE(cOperator, "!=", "<>")
      cOperator = REPLACE(cOperator, "!", "<>")
      .      

    IF bField.DataType = "CHARACTER" THEN
    DO:
      /* If user wants to search with matches, then ignore
       * this if the asterisk is at the end. In that case
       * a BEGINS is better because it might use an index. 
       */
      IF INDEX( RIGHT-TRIM(cValue,"*") ,"*") > 0 THEN
        ASSIGN cOperator = "MATCHES".
      ELSE 
        ASSIGN cValue = RIGHT-TRIM(cValue,"*").
               
      IF cOperator = "" THEN cOperator = "BEGINS".
    END.
    ELSE 
      IF cOperator = "" THEN cOperator = "=".

    pcFilter = SUBSTITUTE("&1 AND &2 &3 &4"
                      , pcFilter
                      , bField.fieldName
                      , cOperator
                      , QUOTER(cValue)
                      ).
  END.

END PROCEDURE. /* getFilter*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initObject wQuery 
PROCEDURE initObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.

  giLightGray = getLightGray().

END PROCEDURE. /* initObject */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openQuery wQuery 
PROCEDURE openQuery :
/* (re) open the query
*/
  DEFINE INPUT PARAMETER phQuery AS HANDLE NO-UNDO.
  
  phQuery:QUERY-OPEN().

END PROCEDURE. /* openQuery */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopenQuery wQuery 
PROCEDURE reopenQuery :
/* Reopen the browse, but take into account 
** the filters and the sort the user applied
*/
  DEFINE VARIABLE cQuery   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iWord    AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lPrepare AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE rCurrent AS ROWID       NO-UNDO.  
  DEFINE VARIABLE cFilter  AS CHARACTER   NO-UNDO.
  
  /* Get the current query */
  cQuery = gcBaseQuery.
  
  /* Remember record we're on */
  IF ghDataBrowse:NUM-SELECTED-ROWS > 0 THEN 
    rCurrent = ghDataBrowse:QUERY:GET-BUFFER-HANDLE(1):ROWID.
  
  /* Get user filter */
  RUN getFilter(OUTPUT cFilter).
  
  /* Find out if it is already sorted */
  IF gcSortColumn <> ? AND gcSortColumn <> "" THEN
  DO:
    IF LOOKUP('BY',cQuery,' ') > 0 THEN
      cQuery = SUBSTITUTE('&1 &2 BY &3 &4 INDEXED-REPOSITION'
                         , TRIM(SUBSTRING(cQuery,1,INDEX(cQuery,' BY ')))
                         , cFilter
                         , gcSortColumn 
                         , TRIM(STRING(glSortAscending,'/DESCENDING'))
                         ).
    ELSE
      cQuery = SUBSTITUTE('&1 &2 BY &3 &4 INDEXED-REPOSITION'
                         , TRIM(SUBSTRING(cQuery,1,INDEX(cQuery,' INDEXED-REPOSITION')))
                         , cFilter
                         , gcSortColumn 
                         , TRIM(STRING(glSortAscending,'/DESCENDING'))
                         ).
  END.
  ELSE 
  DO:
    cQuery = SUBSTITUTE('&1 &2 INDEXED-REPOSITION'
                       , TRIM(SUBSTRING(cQuery,1,INDEX(cQuery,' INDEXED-REPOSITION')))
                       , cFilter
                       ).
  END.

  /* Reopen the query */
  lPrepare = ghDataBrowse:QUERY:QUERY-PREPARE(cQuery) NO-ERROR.
  
  IF NOT lPrepare THEN
  DO:
    cQuery = TRIM(SUBSTRING(cQuery,1,INDEX(cQuery,' BY '))).
    lPrepare = ghDataBrowse:QUERY:QUERY-PREPARE(cQuery) NO-ERROR.    
  END.
  
  IF lPrepare THEN ghDataBrowse:QUERY:QUERY-OPEN().

  /* Find out sort field / ascending to set the sort arrow */
  iWord = LOOKUP('BY',cQuery,' ').
  gcSortColumn    = (IF iWord > 0 THEN ENTRY(iWord + 1,cQuery,' ') ELSE '').
  glSortAscending = (IF iWord > 0 THEN (LOOKUP('DESCENDING',cQuery,' ') = 0) ELSE FALSE).

  /* Set the sort arrow to the right column */
  RUN setSortArrow(gcSortColumn, glSortAscending).

  /* Jump back to selected row */
  IF NOT ghDataBrowse:QUERY:QUERY-OFF-END
    AND rCurrent <> ? THEN
  DO:
    ghDataBrowse:QUERY:REPOSITION-TO-ROWID(rCurrent) NO-ERROR.
    ghDataBrowse:SELECT-FOCUSED-ROW().
  END.

END PROCEDURE. /* reopenQuery */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resizeFilterFields wQuery 
PROCEDURE resizeFilterFields :
/* Redraw the browse filter fields
  */
  DEFINE INPUT PARAMETER pcFilterFields AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pcButtons      AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER phBrowse       AS HANDLE      NO-UNDO.

  DEFINE VARIABLE iField        AS INTEGER NO-UNDO.
  DEFINE VARIABLE iButton       AS INTEGER NO-UNDO.
  DEFINE VARIABLE iCurrentPos   AS INTEGER NO-UNDO.
  DEFINE VARIABLE iRightEdge    AS INTEGER NO-UNDO.
  DEFINE VARIABLE iWidth        AS INTEGER NO-UNDO.
  DEFINE VARIABLE hColumn       AS HANDLE  NO-UNDO.
  DEFINE VARIABLE hButton       AS HANDLE  NO-UNDO.
  DEFINE VARIABLE hFilterField  AS HANDLE  NO-UNDO.
  DEFINE VARIABLE iFilter       AS INTEGER NO-UNDO.

  /* To prevent drawing error, make all fields small */
  DO iField = 1 TO NUM-ENTRIES(pcFilterFields):
    hFilterField = HANDLE(ENTRY(iField,pcFilterFields)).
    hFilterField:VISIBLE      = NO.
    hFilterField:X            = phBrowse:X.
    hFilterField:Y            = phBrowse:Y - 23.
    hFilterField:WIDTH-PIXELS = 1.
  END.

  /* Start by setting the buttons at the proper place. Do this right to left */
  ASSIGN iRightEdge = phBrowse:X + phBrowse:WIDTH-PIXELS.
  DO iButton = NUM-ENTRIES(pcButtons) TO 1 BY -1:
    hButton = HANDLE(ENTRY(iButton,pcButtons)).
    hButton:X = iRightEdge - hButton:WIDTH-PIXELS.
    hButton:Y = phBrowse:Y - 23. /* filter buttons close to the browse */
    iRightEdge = hButton:X + 0. /* A little margin between buttons */
  END.

  /* The left side of the left button is the maximum point
   * Fortunately, this value is already in iRightEdge.
   * Resize and reposition the fields from left to right,
   * use the space between browse:x and iRightEdge
   */

  /* Take the left side of the first visible column as a starting point. */
  firstVisibleColumn:
  DO iField = 1 TO phBrowse:NUM-COLUMNS:
    hColumn = phBrowse:GET-BROWSE-COLUMN(iField):HANDLE.

    IF hColumn:X > 0 AND hColumn:VISIBLE THEN
    DO:
      iCurrentPos = phBrowse:X + hColumn:X.
      LEAVE firstVisibleColumn.
    END.
  END.

  #Field:
  DO iField = 1 TO phBrowse:NUM-COLUMNS:
    hColumn = phBrowse:GET-BROWSE-COLUMN(iField):handle.

    /* Some types cannot have a filter */
    IF hColumn:DATA-TYPE = 'raw' THEN NEXT #Field.

    iFilter = iFilter + 1.
    IF iFilter > NUM-ENTRIES(pcFilterFields) THEN LEAVE #Field.

    /* Determine the handle of the filterfield */
    hFilterField = HANDLE(ENTRY(iFilter, pcFilterFields)).

    /* If the column is hidden, make the filter hidden and go to the next */
    IF NOT hColumn:VISIBLE THEN
    DO:
      hFilterField:VISIBLE = NO.
      NEXT #Field.
    END.

    /* Where *are* we ?? */
    iCurrentPos = phBrowse:X + hColumn:X.

    /* If the columns have been resized, some columns might have fallen off the screen */
    IF hColumn:X < 1 THEN NEXT #Field.

    /* Does it fit on the screen? */
    IF iCurrentPos >= iRightEdge - 5 THEN LEAVE #Field. /* accept some margin */

    /* Where will this field end? And does it fit? */
    iWidth = hColumn:WIDTH-PIXELS + 4.
    IF iCurrentPos + iWidth > iRightEdge THEN iWidth = iRightEdge - iCurrentPos.

    /* Ok, seems to fit */
    hFilterField:X            = iCurrentPos.
    hFilterField:WIDTH-PIXELS = iWidth.
    iCurrentPos               = iCurrentPos + iWidth.
    hFilterField:VISIBLE      = phBrowse:VISIBLE. /* take over the visibility of the browse */
  END.

END PROCEDURE. /* resizeFilterFields */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resizeFilters wQuery 
PROCEDURE resizeFilters :
/* Redraw the filters. This is needed when the window resizes, one of
 * the columns resizes or the user scrolls in the browse.
 */
  DEFINE VARIABLE cFilterFields AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cButtons      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE hBrowse       AS HANDLE    NO-UNDO.
  DEFINE VARIABLE iField        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE hField        AS HANDLE    NO-UNDO.
  
  DEFINE BUFFER bField FOR ttField.
  
  getFilterLoop:
  FOR EACH bField BY bField.orderNr:
    IF VALID-HANDLE(bField.hFilter) THEN
      cFilterFields = TRIM(SUBSTITUTE('&1,&2', cFilterFields, bField.hFilter),',').
  END.

  DO WITH FRAME frData:
    cButtons = SUBSTITUTE('&1', btnClearDataFilter:HANDLE).
  END.

  /* Resize them */
  RUN resizeFilterFields
    ( INPUT cFilterFields
    , INPUT cButtons
    , INPUT hBrowse
    ).

END PROCEDURE. /* resizeFilters */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resizeWindow wQuery 
PROCEDURE resizeWindow :
DEFINE VARIABLE hWindow AS HANDLE  NO-UNDO.
  DEFINE VARIABLE hFrame  AS HANDLE  NO-UNDO.

  ASSIGN 
    hWindow = wQuery:HANDLE
    hFrame  = FRAME frData:HANDLE.

  /* Use a minimum size to avoid drawing errors */
  IF hWindow:WIDTH-PIXELS < 200 OR hWindow:HEIGHT-PIXELS < 100 THEN RETURN. 

  RUN lockWindow.p(INPUT hWindow, YES).

  /* Adapt the frame */
  hFrame:SCROLLABLE            = YES.
  hFrame:VIRTUAL-WIDTH-PIXELS  = SESSION:WIDTH-PIXELS.
  hFrame:VIRTUAL-HEIGHT-PIXELS = SESSION:HEIGHT-PIXELS.
  hFrame:WIDTH-PIXELS          = hWindow:WIDTH-PIXELS - hFrame:X - 2.
  hFrame:HEIGHT-PIXELS         = hWindow:HEIGHT-PIXELS - hFrame:Y - 2.

  /* Adapt the browse */
  ghDataBrowse:WIDTH-PIXELS  = hFrame:WIDTH-PIXELS - ghDataBrowse:X - 2.
  ghDataBrowse:HEIGHT-PIXELS = hFrame:HEIGHT-PIXELS - ghDataBrowse:Y - 2.

  RUN dataScrollNotify(ghDataBrowse).

  /* Set frame back */
  hFrame:VIRTUAL-WIDTH-PIXELS  = hFrame:WIDTH-PIXELS. 
  hFrame:VIRTUAL-HEIGHT-PIXELS = hFrame:HEIGHT-PIXELS.
  hFrame:SCROLLABLE            = NO.

  RUN lockWindow.p(INPUT hWindow, NO).

END PROCEDURE. /* resizeWindow */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE screenHide wQuery 
PROCEDURE screenHide :
/* Procedure that runs on hide 
*/
  DEFINE BUFFER bField FOR ttField.

  /* Clean up the filters */
  FOR EACH bField:
    IF VALID-HANDLE(bField.hFilter) THEN
      DELETE OBJECT bField.hFilter NO-ERROR.
  END.

END PROCEDURE. /* ScreenHide */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScreenInit wQuery 
PROCEDURE ScreenInit :
/* Bind the dataset to the screen
*/
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsQuery BIND.
  
  DO WITH FRAME {&frame-name}:
    btnClearDataFilter:LOAD-IMAGE('image\clear.gif').
  END.
  
END PROCEDURE. /* ScreenInit */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE screenShow wQuery 
PROCEDURE screenShow :
/* Procedure that runs on show of tab
*/
  FIND ttQuery.
  RUN buildQuery(OUTPUT ttQuery.hQuery).
  gcBaseQuery = ttQuery.hQuery:PREPARE-STRING.
  
  RUN buildBrowse(ttQuery.hQuery).
  RUN openQuery(ttQuery.hQuery).
  
END PROCEDURE. /* ScreenShow */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSortArrow wQuery 
PROCEDURE setSortArrow :
/* Set a sorting arrow in the browse
*/
  DEFINE INPUT PARAMETER pcColumn    AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER plAscending AS LOGICAL     NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER     NO-UNDO.
  
  #SetSort:
  DO i = 1 TO ghDataBrowse:NUM-COLUMNS:

    IF ghDataBrowse:GET-BROWSE-COLUMN(i):NAME = pcColumn THEN 
      ghDataBrowse:SET-SORT-ARROW(i, plAscending ).
    ELSE 
      ghDataBrowse:SET-SORT-ARROW(i, ? ). /* erase existing arrow */
  END.
  
END PROCEDURE. /* setSortArrow */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showQuery wQuery 
PROCEDURE showQuery :
/* Show the current query
*/
  IF VALID-HANDLE(ghDataBrowse) 
    AND VALID-HANDLE(ghDataBrowse:QUERY) THEN   
    MESSAGE ghDataBrowse:QUERY:PREPARE-STRING VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sizeToFit wQuery 
PROCEDURE sizeToFit :
/* Adjust the window to be the same size as the browse
*/
  DEFINE VARIABLE iTotalWidth AS INTEGER     NO-UNDO.
  
  DEFINE BUFFER bField FOR ttField.

  FOR EACH bField WHERE VALID-HANDLE(bField.hColumn):
    iTotalWidth = iTotalWidth + bField.hColumn:WIDTH-PIXELS.
  END.

  iTotalWidth = iTotalWidth + 60.
  IF iTotalWidth < SESSION:WIDTH-PIXELS THEN 
  DO:
    {&window-name}:WIDTH-PIXELS = iTotalWidth.
    RUN resizeWindow.
  END.

END PROCEDURE. /* sizeToFit */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION createMenu wQuery 
FUNCTION createMenu RETURNS HANDLE
  ( phParent AS HANDLE ) :

  DEFINE VARIABLE hMenu AS HANDLE NO-UNDO.

  IF VALID-HANDLE(phParent) THEN
    hMenu = phParent:POPUP-MENU.

  /* Kill the current menu */
  IF VALID-HANDLE(hMenu) THEN killMenu(hMenu).

  /* Create the menu itself */
  CREATE MENU hMenu
    ASSIGN
      POPUP-ONLY = TRUE
      SENSITIVE  = TRUE
    TRIGGERS:
      ON "menu-drop" PERSISTENT RUN menuDropDataBrowse IN THIS-PROCEDURE. /* enable/disable menu-items */
    END TRIGGERS.

  IF VALID-HANDLE(phParent) THEN
    phParent:POPUP-MENU = hMenu.

  RETURN hMenu.

END FUNCTION. /* createMenu */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION createMenuItem wQuery 
FUNCTION createMenuItem RETURNS HANDLE
  ( phMenu    AS HANDLE
  , pcType    AS CHARACTER
  , pcLabel   AS CHARACTER
  , pcName    AS CHARACTER
  ) :

  DEFINE VARIABLE hMenuItem AS HANDLE NO-UNDO.

  CASE pcType:
    WHEN "SUBMENU" THEN
      CREATE SUB-MENU hMenuItem
        ASSIGN
          LABEL        = pcLabel
          PRIVATE-DATA = pcLabel
          NAME         = pcName
          PARENT       = phMenu.

    WHEN "TOGGLE-BOX" THEN
      CREATE MENU-ITEM hMenuItem
        ASSIGN
          LABEL        = pcLabel
          PRIVATE-DATA = pcLabel
          NAME         = pcName
          TOGGLE-BOX   = TRUE
          CHECKED      = TRUE
          PARENT       = phMenu.

    WHEN "RULE" THEN
      CREATE MENU-ITEM hMenuItem
        ASSIGN
          SUBTYPE      = "rule"
          PARENT       = phMenu.

    OTHERWISE
      CREATE MENU-ITEM hMenuItem
        ASSIGN
          LABEL        = pcLabel
          PRIVATE-DATA = pcLabel
          NAME         = pcName
          PARENT       = phMenu.

  END CASE.


  RETURN hMenuItem.

END FUNCTION. /* createMenuItem */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION filterModified wQuery 
FUNCTION filterModified RETURNS LOGICAL
  ( phFilterField AS HANDLE
  , plModified    AS LOGICAL ) :
  /* Set modified-flag for a filter field
   */
  DEFINE BUFFER bField FOR ttField.

  FIND FIRST bField WHERE bField.hFilter = phFilterField NO-ERROR.

  IF AVAILABLE bField THEN
  DO:
    IF plModified <> ? THEN bField.lModified = plModified.
    RETURN bField.lModified.
  END.
  ELSE
    RETURN ?.

END FUNCTION. /* FilterModified */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKeyList wQuery 
FUNCTION getKeyList RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
  /* Return a list of special keys pressed
  */
  DEFINE VARIABLE mKeyboardState AS MEMPTR    NO-UNDO.
  DEFINE VARIABLE iReturnValue   AS INT64     NO-UNDO.
  DEFINE VARIABLE cKeyList       AS CHARACTER NO-UNDO.

  SET-SIZE(mKeyboardState) = 256.

  /* Get the current state of the keyboard */
  RUN GetKeyboardState(GET-POINTER-VALUE(mKeyboardState), OUTPUT iReturnValue) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN ''. /* try to suppress error: 'C' Call Stack has been compromised after calling  in  (6069) */

  IF GET-BITS(GET-BYTE(mKeyboardState, 1 + 16), 8, 1) = 1 THEN cKeyList = TRIM(cKeyList + ",SHIFT",",").
  IF GET-BITS(GET-BYTE(mKeyboardState, 1 + 17), 8, 1) = 1 THEN cKeyList = TRIM(cKeyList + ",CTRL",",").
  IF GET-BITS(GET-BYTE(mKeyboardState, 1 + 18), 8, 1) = 1 THEN cKeyList = TRIM(cKeyList + ",ALT",",").

  FINALLY:
    SET-SIZE(mKeyboardState) = 0.
    RETURN cKeyList.   /* Function return value. */
  END FINALLY.

END FUNCTION. /* getKeyList */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSafeFormat wQuery 
FUNCTION getSafeFormat RETURNS CHARACTER
  ( pcFormat   AS CHARACTER 
  , pcDataType AS CHARACTER ) :

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  /* Autocorrect 2-digit years in date fields */
  IF pcDataType = "DATE"
    AND pcFormat MATCHES "99.99.99" THEN pcFormat = pcFormat + "99".

  /* Protect against "value could not be displayed using..." errors. */
  IF (   pcDataType = "DECIMAL"
      OR pcDataType = "RECID"
      OR pcDataType BEGINS "INT") /* Use BEGINS to cover integer / int64 and extents of both */
     AND NOT pcFormat BEGINS "HH:MM"   /* Skip time fields */ THEN
  DO:
    /* Add minus sign if needed. Because a format can contain extra characters like "$"
     * we need to find out what the right place might be to add it. If the format begins
     * with extra chars, we add it at the back. But if there are extra chars too, we just
     * cannot add the extra minus char. So be it :(
     */
    IF INDEX(pcFormat,"-") = 0
      AND INDEX(pcFormat,"+") = 0 THEN
    DO:
      IF pcFormat BEGINS '9' OR pcFormat BEGINS '>' THEN pcFormat = "-" + pcFormat.
      ELSE
      IF pcFormat MATCHES '*9' THEN pcFormat = pcFormat + "-".
    END.

    /* Add extra digit placeholders */
    addDigits:
    DO i = 1 TO LENGTH(pcFormat):
      IF LOOKUP(SUBSTRING(pcFormat,i,1),">,Z,9") > 0 THEN
      DO:
        IF i = 1 THEN
          pcFormat = ">>>>>>>>>>>>>>>" + pcFormat.
        ELSE
          pcFormat = SUBSTRING(pcFormat,1,i - 1) + ">>>>>>>>>>>>>>>" + SUBSTRING(pcFormat,i).
        LEAVE addDigits.
      END.
    END.
  END.

  RETURN pcFormat. /* Function return value. */

END FUNCTION. /* getSafeFormat */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION killMenu wQuery 
FUNCTION killMenu RETURNS LOGICAL
  ( phMenu AS HANDLE ) :

  DEFINE VARIABLE hItemToDelete AS HANDLE NO-UNDO.
  DEFINE VARIABLE hMenuItem     AS HANDLE NO-UNDO.

  IF VALID-HANDLE(phMenu) THEN
  DO:
    /* Delete a menu and all of its siblings
     */
    hMenuItem = phMenu:FIRST-CHILD.

    /* Kill subitems */
    DO WHILE VALID-HANDLE(hMenuItem):
      IF hMenuItem:DYNAMIC THEN hItemToDelete = hMenuItem.
      hMenuItem = hMenuItem:NEXT-SIBLING.
      IF VALID-HANDLE(hItemToDelete) THEN
        DELETE OBJECT hItemToDelete NO-ERROR.
    END.

    /* Kill the menu itself */
    DELETE OBJECT phMenu NO-ERROR.
  END.

  RETURN TRUE.   /* Function return value. */

END FUNCTION. /* killMenu */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION lockWindow wQuery 
FUNCTION lockWindow RETURNS LOGICAL
  ( plLockWindow AS LOGICAL ) :
  /* Lock/unlock the window, keep track of nested locks/unlocks
  */
  giWindowLockCounter = giWindowLockCounter + (IF plLockWindow THEN 1 ELSE -1).

  IF giWindowLockCounter > 0 THEN 
    RUN lockWindow.p(INPUT {&WINDOW-NAME}:HANDLE, TRUE).
  ELSE 
    RUN lockWindow.p(INPUT {&WINDOW-NAME}:HANDLE, FALSE).

  RETURN TRUE.

END FUNCTION. /* lockWindow */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFilterFieldColor wQuery 
FUNCTION setFilterFieldColor RETURNS LOGICAL
  ( phWidget AS HANDLE ) :

  /* Set color to gray if not entered a text manually
  */
  IF phWidget:SCREEN-VALUE = phWidget:PRIVATE-DATA
    AND FilterModified(phWidget,?) = FALSE THEN
    phWidget:FGCOLOR = 7.
  ELSE
    phWidget:FGCOLOR = ?.

  phWidget:BGCOLOR = 15.
  RETURN TRUE.

END FUNCTION. /* setFilterFieldColor */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
