&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
    File        : wModifyTables.w
    Purpose     : Modify tables of a query

    Author(s)   : Patrick Tingen
    Created     : 2019

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

{queryLib.i &reference-only=reference-only}

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_IS_RUNNING) = 0 &THEN
  DEFINE INPUT  PARAMETER phParent AS HANDLE NO-UNDO.
  DEFINE OUTPUT PARAMETER phFrame  AS HANDLE NO-UNDO.
&ELSE
  DEFINE VARIABLE phParent AS HANDLE NO-UNDO.
  DEFINE VARIABLE phFrame  AS HANDLE NO-UNDO.
  
  phParent = SESSION:HANDLE.  
  CURRENT-WINDOW:WIDTH = 200.
  CURRENT-WINDOW:HEIGHT = 15.   
&ENDIF

DEFINE VARIABLE giLightGray AS INTEGER NO-UNDO.
DEFINE VARIABLE giRowNr     AS INTEGER NO-UNDO EXTENT 2.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brAvailable

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttSchemaTable

/* Definitions for BROWSE brAvailable                                   */
&Scoped-define FIELDS-IN-QUERY-brAvailable ttSchemaTable.cTableName ttSchemaTable.cTableDesc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brAvailable   
&Scoped-define SELF-NAME brAvailable
&Scoped-define OPEN-QUERY-brAvailable DO WITH FRAME {&FRAME-NAME}:   OPEN QUERY {&SELF-NAME}   FOR EACH ttSchemaTable     WHERE (    ttSchemaTable.lSelected = FALSE            AND ttSchemaTable.cTableName MATCHES '*' + fiFilterAvailable:SCREEN-VALUE + '*')        OR           (    ttSchemaTable.lSelected = FALSE            AND ttSchemaTable.cTableDesc MATCHES '*' + fiFilterAvailable:SCREEN-VALUE + '*')     BY ttSchemaTable.cTableName.  END.
&Scoped-define TABLES-IN-QUERY-brAvailable ttSchemaTable
&Scoped-define FIRST-TABLE-IN-QUERY-brAvailable ttSchemaTable


/* Definitions for BROWSE brSelected                                    */
&Scoped-define FIELDS-IN-QUERY-brSelected ttSchemaTable.cTableName ttSchemaTable.cTableDesc ttSchemaTable.cLinkedTo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brSelected   
&Scoped-define SELF-NAME brSelected
&Scoped-define OPEN-QUERY-brSelected DO WITH FRAME {&FRAME-NAME}:   OPEN QUERY {&SELF-NAME}   FOR EACH ttSchemaTable     WHERE (    ttSchemaTable.lSelected = TRUE            AND ttSchemaTable.cTableName MATCHES '*' + fiFilterSelected:SCREEN-VALUE + '*')        OR           (    ttSchemaTable.lSelected = TRUE            AND ttSchemaTable.cTableDesc MATCHES '*' + fiFilterSelected:SCREEN-VALUE + '*')         BY ttSchemaTable.iOrder. END.
&Scoped-define TABLES-IN-QUERY-brSelected ttSchemaTable
&Scoped-define FIRST-TABLE-IN-QUERY-brSelected ttSchemaTable


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brAvailable}~
    ~{&OPEN-QUERY-brSelected}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnDeselect fiFilterAvailable ~
fiFilterSelected brAvailable brSelected btnDown btnSelect btnUp 
&Scoped-Define DISPLAYED-OBJECTS fiFilterAvailable fiFilterSelected 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnDeselect  NO-FOCUS FLAT-BUTTON
     LABEL "<=" 
     SIZE 7 BY 1.67.

DEFINE BUTTON btnDown  NO-FOCUS FLAT-BUTTON
     LABEL "Down" 
     SIZE 7 BY 1.67.

DEFINE BUTTON btnSelect  NO-FOCUS FLAT-BUTTON
     LABEL "=>" 
     SIZE 7 BY 1.67.

DEFINE BUTTON btnUp  NO-FOCUS FLAT-BUTTON
     LABEL "Up" 
     SIZE 7 BY 1.67.

DEFINE VARIABLE fiFilterAvailable AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 80 BY 1 NO-UNDO.

DEFINE VARIABLE fiFilterSelected AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 90 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brAvailable FOR 
      ttSchemaTable SCROLLING.

DEFINE QUERY brSelected FOR 
      ttSchemaTable SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brAvailable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brAvailable C-Win _FREEFORM
  QUERY brAvailable DISPLAY
      ttSchemaTable.cTableName
ttSchemaTable.cTableDesc
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 80 BY 13.33 FIT-LAST-COLUMN.

DEFINE BROWSE brSelected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brSelected C-Win _FREEFORM
  QUERY brSelected DISPLAY
      ttSchemaTable.cTableName
ttSchemaTable.cTableDesc
ttSchemaTable.cLinkedTo
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 90 BY 13.33 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnDeselect AT ROW 8 COL 85 WIDGET-ID 4
     fiFilterAvailable AT ROW 1.24 COL 2 NO-LABEL WIDGET-ID 6
     fiFilterSelected AT ROW 1.24 COL 94 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     brAvailable AT ROW 2.43 COL 2 WIDGET-ID 200
     brSelected AT ROW 2.43 COL 96 WIDGET-ID 300
     btnDown AT ROW 8 COL 189 WIDGET-ID 22
     btnSelect AT ROW 6 COL 85 WIDGET-ID 2
     btnUp AT ROW 6 COL 189 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 200 BY 15 WIDGET-ID 100.


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
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 15
         WIDTH              = 200
         MAX-HEIGHT         = 15
         MAX-WIDTH          = 200
         VIRTUAL-HEIGHT     = 15
         VIRTUAL-WIDTH      = 200
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
ASSIGN C-Win = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB brAvailable fiFilterSelected DEFAULT-FRAME */
/* BROWSE-TAB brSelected brAvailable DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN fiFilterAvailable IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brAvailable
/* Query rebuild information for BROWSE brAvailable
     _START_FREEFORM
DO WITH FRAME {&FRAME-NAME}:
  OPEN QUERY {&SELF-NAME}
  FOR EACH ttSchemaTable
    WHERE (    ttSchemaTable.lSelected = FALSE
           AND ttSchemaTable.cTableName MATCHES '*' + fiFilterAvailable:SCREEN-VALUE + '*')
       OR
          (    ttSchemaTable.lSelected = FALSE
           AND ttSchemaTable.cTableDesc MATCHES '*' + fiFilterAvailable:SCREEN-VALUE + '*')
    BY ttSchemaTable.cTableName.

END.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brAvailable */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brSelected
/* Query rebuild information for BROWSE brSelected
     _START_FREEFORM
DO WITH FRAME {&FRAME-NAME}:
  OPEN QUERY {&SELF-NAME}
  FOR EACH ttSchemaTable
    WHERE (    ttSchemaTable.lSelected = TRUE
           AND ttSchemaTable.cTableName MATCHES '*' + fiFilterSelected:SCREEN-VALUE + '*')
       OR
          (    ttSchemaTable.lSelected = TRUE
           AND ttSchemaTable.cTableDesc MATCHES '*' + fiFilterSelected:SCREEN-VALUE + '*')

       BY ttSchemaTable.iOrder.
END.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brSelected */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brAvailable
&Scoped-define SELF-NAME brAvailable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brAvailable C-Win
ON OFF-HOME OF brAvailable IN FRAME DEFAULT-FRAME
DO:
  APPLY 'entry' TO fiFilterAvailable.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brAvailable C-Win
ON ROW-DISPLAY OF brAvailable IN FRAME DEFAULT-FRAME
DO:
  DEFINE VARIABLE iColor AS INTEGER NO-UNDO.
  
  giRowNr[1] = giRowNr[1] + 1.
  iColor = (IF giRowNr[1] MODULO 2 = 1 THEN ? ELSE giLightGray).
  ttSchemaTable.cTableName:BGCOLOR IN BROWSE {&Browse-name} = iColor.
  ttSchemaTable.cTableDesc:BGCOLOR IN BROWSE {&Browse-name} = iColor.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brSelected
&Scoped-define SELF-NAME brSelected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brSelected C-Win
ON OFF-HOME OF brSelected IN FRAME DEFAULT-FRAME
DO:
  APPLY 'entry' TO fiFilterSelected.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brSelected C-Win
ON ROW-DISPLAY OF brSelected IN FRAME DEFAULT-FRAME
DO:
  DEFINE VARIABLE iColor AS INTEGER NO-UNDO.
  
  giRowNr[2] = giRowNr[2] + 1.
  iColor = (IF giRowNr[2] MODULO 2 = 1 THEN ? ELSE giLightGray).
  ttSchemaTable.cTableName:BGCOLOR IN BROWSE {&Browse-name} = iColor.
  ttSchemaTable.cTableDesc:BGCOLOR IN BROWSE {&Browse-name} = iColor.
  ttSchemaTable.cLinkedTo :BGCOLOR IN BROWSE {&Browse-name} = iColor.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeselect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeselect C-Win
ON CHOOSE OF btnDeselect IN FRAME DEFAULT-FRAME /* <= */
OR 'default-action' OF brSelected
OR 'delete' OF brSelected
DO:
  RUN selectTable(brSelected:HANDLE,brAvailable:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDown C-Win
ON CHOOSE OF btnDown IN FRAME DEFAULT-FRAME /* Down */
OR 'ctrl-cursor-down' OF brSelected
DO:
  brSelected:FETCH-SELECTED-ROW(1).
  IF AVAILABLE ttSchemaTable THEN 
    RUN moveTable(ttSchemaTable.cTableName, NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelect C-Win
ON CHOOSE OF btnSelect IN FRAME DEFAULT-FRAME /* => */
OR 'default-action' OF brAvailable
OR 'insert' OF brAvailable
DO:
  RUN selectTable(brAvailable:HANDLE, brSelected:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUp C-Win
ON CHOOSE OF btnUp IN FRAME DEFAULT-FRAME /* Up */
OR 'ctrl-cursor-up' OF brSelected
DO:
  brSelected:FETCH-SELECTED-ROW(1).
  IF AVAILABLE ttSchemaTable THEN 
    RUN moveTable(ttSchemaTable.cTableName, YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFilterAvailable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFilterAvailable C-Win
ON CURSOR-DOWN OF fiFilterAvailable IN FRAME DEFAULT-FRAME
DO:
  APPLY 'entry' TO brAvailable.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFilterAvailable C-Win
ON VALUE-CHANGED OF fiFilterAvailable IN FRAME DEFAULT-FRAME
DO:
  {&OPEN-QUERY-brSelected}
  {&OPEN-QUERY-brAvailable}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFilterSelected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFilterSelected C-Win
ON CURSOR-DOWN OF fiFilterSelected IN FRAME DEFAULT-FRAME
DO:
  APPLY 'entry' TO brSelected.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFilterSelected C-Win
ON VALUE-CHANGED OF fiFilterSelected IN FRAME DEFAULT-FRAME
DO:
  {&OPEN-QUERY-brSelected}
  {&OPEN-QUERY-brAvailable}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brAvailable
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

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   
  RUN enable_UI.
  phFrame = FRAME {&FRAME-NAME}:HANDLE.
  
END.

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
  /* Hide all frames. */
  HIDE FRAME DEFAULT-FRAME.
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
  DISPLAY fiFilterAvailable fiFilterSelected 
      WITH FRAME DEFAULT-FRAME.
  ENABLE btnDeselect fiFilterAvailable fiFilterSelected brAvailable brSelected 
         btnDown btnSelect btnUp 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getParents C-Win 
PROCEDURE getParents :
/* Try to automatically get the parent of the selected tables
*/
  DEFINE BUFFER bParent FOR ttSchemaTable.
  DEFINE BUFFER bChild  FOR ttSchemaTable.
  
  FOR EACH bParent WHERE bParent.lSelected = TRUE:
    ASSIGN bParent.cLinkedTo = ''.
  END.
  
  FOR EACH bParent WHERE bParent.lSelected = TRUE:
  
    #tableLoop:
    FOR EACH bChild 
      WHERE bChild.lSelected = TRUE 
        AND bChild.iOrder    > bParent.iOrder
        AND bChild.cLinkedTo = '':

      IF canHaveRelation(bParent.cTablename, bChild.cTablename) THEN
      DO:
        ASSIGN bChild.cLinkedTo = bParent.cTablename.          
        NEXT #tableLoop.
      END. /* can have relation */
    END. /* for each child */
  END. /* for each parent */

END PROCEDURE. /* getParents */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScreenInit C-Win 
PROCEDURE ScreenInit :
/* Bind the dataset to the screen
*/
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsQuery BIND.
  
  DO WITH FRAME {&frame-name}:
  
    /* Get the schema of all connected databases.
    ** We only need to do this on startup
    */                                     
    RUN getSchema(OUTPUT DATASET dsSchema BY-REFERENCE).
    
    giLightGray = getLightGray().
    
    /* Load images */
    btnSelect  :LOAD-IMAGE('image\right.gif').
    btnDeselect:LOAD-IMAGE('image\left.gif').
    btnUp      :LOAD-IMAGE('image\up.gif').
    btnDown    :LOAD-IMAGE('image\down.gif').

  END.
  
END PROCEDURE. /* ScreenInit */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveTable C-Win 
PROCEDURE moveTable :
/* Move table up or down
*/
  DEFINE INPUT PARAMETER pcTable    AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER plMoveUp   AS LOGICAL     NO-UNDO.

  DEFINE VARIABLE iTemp AS INTEGER     NO-UNDO.
  DEFINE BUFFER bTable1 FOR ttSchemaTable.
  DEFINE BUFFER bTable2 FOR ttSchemaTable.
  
  /* Find org table */
  FIND bTable1 
    WHERE bTable1.cTableName = pcTable NO-ERROR.
  IF NOT AVAILABLE bTable1 THEN RETURN. 
  
  IF plMoveUp THEN
    FIND LAST bTable2 
      WHERE bTable2.lSelected = TRUE
        AND bTable2.iOrder    < bTable1.iOrder NO-ERROR.
  ELSE
    FIND FIRST bTable2 
      WHERE bTable2.lSelected = TRUE
        AND bTable2.iOrder    > bTable1.iOrder NO-ERROR.
  
  IF AVAILABLE bTable2 THEN 
  DO:
    iTemp = bTable1.iOrder.
    bTable1.iOrder = bTable2.iOrder.
    bTable2.iOrder = iTemp.  
  END.

  RUN getParents.
  RUN saveData.
  RUN reopenBrowses.
  
END PROCEDURE. /* moveTable */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopenBrowses C-Win 
PROCEDURE reopenBrowses :
/* Reopen table browses
*/
  DEFINE VARIABLE rAvailable AS ROWID NO-UNDO.
  DEFINE VARIABLE rSelected  AS ROWID NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF brAvailable:QUERY:NUM-RESULTS > 0 THEN 
    DO:
      BROWSE brAvailable:FETCH-SELECTED-ROW(1).
      IF AVAILABLE ttSchemaTable THEN rAvailable = ROWID(ttSchemaTable).
    END.
    
    IF brSelected:QUERY:NUM-RESULTS > 0 THEN 
    DO:
      BROWSE brSelected:FETCH-SELECTED-ROW(1).
      IF AVAILABLE ttSchemaTable THEN rSelected = ROWID(ttSchemaTable).
    END.
    
    {&OPEN-QUERY-brSelected}
    {&OPEN-QUERY-brAvailable}  
    
    IF rAvailable <> ? THEN BROWSE brAvailable:QUERY:REPOSITION-TO-ROWID(rAvailable) NO-ERROR.
    IF rSelected  <> ? THEN BROWSE brSelected:QUERY:REPOSITION-TO-ROWID(rSelected) NO-ERROR.  
  END.

END PROCEDURE. /* reopenBrowses */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveData C-Win 
PROCEDURE saveData :
/* save current state of tables in the dataset
*/
  DEFINE BUFFER bTable FOR ttTable.
  DEFINE BUFFER bQuery FOR ttQuery.
  DEFINE BUFFER bField FOR ttField. 
  
  FOR EACH ttSchemaTable:
  
    FIND bQuery. 
    FIND bTable WHERE bTable.tableName = ttSchemaTable.cTableName NO-ERROR.
    
    /* Preserve order of tables */
    IF AVAILABLE bTable THEN bTable.orderNr = ttSchemaTable.iOrder.
    
    /* Table got selected */
    IF ttSchemaTable.lSelected = TRUE AND NOT AVAILABLE bTable THEN
    DO:
      CREATE bTable.
      ASSIGN
        bTable.queryNr     = bQuery.queryNr
        bTable.tableName   = ttSchemaTable.cTableName
        bTable.parentTable = ttSchemaTable.cLinkedTo
        bTable.autoJoin    = (ttSchemaTable.cLinkedTo <> '')
        bTable.conditions  = ''
        .      
    END. /* selected */
    
    /* Table got deselected */
    IF ttSchemaTable.lSelected = FALSE AND AVAILABLE bTable THEN
    DO:
      /* Remove fields */
      FOR EACH bField WHERE bField.tableName = bTable.tableName:
        DELETE bField.
      END.
      DELETE bTable.
    END. /* deselected */     
  END.     

END PROCEDURE. /* saveData */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScreenValidate C-Win 
PROCEDURE ScreenValidate :
/* Update db with info from screen
*/
  DEFINE OUTPUT PARAMETER pcError AS CHARACTER NO-UNDO.
  
  DO WITH FRAME {&frame-name}:
  
    IF NOT CAN-FIND(FIRST ttSchemaTable
                    WHERE ttSchemaTable.lSelected = TRUE) THEN 
      pcError = 'Please select at least one table'.
    
  END.

END PROCEDURE. /* ScreenValidate */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScreenHide C-Win 
PROCEDURE ScreenHide :
/* Gets called when the user leaves the screen
*/
  RUN saveData.
  
END PROCEDURE. /* ScreenHide */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScreenShow C-Win 
PROCEDURE ScreenShow :
/* Gets called when the frame comes into view
*/
  DEFINE BUFFER bTable FOR ttTable.

  DO WITH FRAME {&frame-name}:
    
    /* Get selected tables */
    FOR EACH ttSchemaTable:
      
      ttSchemaTable.lSelected = FALSE.
      
      FIND bTable WHERE bTable.tableName = ttSchemaTable.cTableName NO-ERROR.
      IF AVAILABLE bTable THEN
      DO:
        ASSIGN 
          ttSchemaTable.lSelected = TRUE
          ttSchemaTable.iOrder    = bTable.orderNr
          ttSchemaTable.cLinkedTo = bTable.parentTable
          .    
      END.
    END.     
    
    RUN reopenBrowses.
    APPLY 'entry' TO brAvailable.
    
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectTable C-Win 
PROCEDURE selectTable :
/* Select / deselect table
*/
  DEFINE INPUT PARAMETER phOldBrowse AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER phNewBrowse AS HANDLE NO-UNDO.

  DEFINE VARIABLE rOldRecord AS ROWID NO-UNDO.
  DEFINE VARIABLE rNewRecord AS ROWID NO-UNDO.
  DEFINE VARIABLE hOldBuffer AS HANDLE      NO-UNDO.
  
  DEFINE BUFFER bTable FOR ttSchemaTable. 
  
  APPLY 'entry' TO phOldBrowse.
  
  /* remember rowid of selected table */
  hOldBuffer = phOldBrowse:QUERY:GET-BUFFER-HANDLE(1).
  
  rOldRecord = hOldBuffer:ROWID.
  IF phOldBrowse:QUERY:NUM-RESULTS = 0 THEN RETURN. 

  /* Determine record for new focus */
  IF phOldBrowse:QUERY:GET-NEXT() THEN
    rNewRecord = hOldBuffer:ROWID.
  ELSE 
  DO:
    phOldBrowse:QUERY:GET-LAST().
    phOldBrowse:QUERY:GET-PREV().
    IF hOldBuffer:AVAILABLE THEN rNewRecord = hOldBuffer:ROWID.
  END.
  
  /* Assign new order nr */
  FIND ttSchemaTable WHERE ROWID(ttSchemaTable) = rOldRecord NO-ERROR.
  FIND LAST bTable WHERE bTable.lSelected = (NOT ttSchemaTable.lSelected) NO-ERROR.    
  ttSchemaTable.lSelected = NOT ttSchemaTable.lSelected.
  ttSchemaTable.iOrder = (IF AVAILABLE bTable THEN bTable.iOrder + 1 ELSE 1).
  
  RUN getParents.
  RUN saveData.

  {&OPEN-QUERY-brSelected}
  {&OPEN-QUERY-brAvailable}  
  
  IF rOldRecord <> ? THEN phNewBrowse:QUERY:REPOSITION-TO-ROWID(rOldRecord) NO-ERROR.
  IF rNewRecord <> ? THEN phOldBrowse:QUERY:REPOSITION-TO-ROWID(rNewRecord) NO-ERROR.

END PROCEDURE. /* selectTable */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
