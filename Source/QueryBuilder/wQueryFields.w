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
&Scoped-define INTERNAL-TABLES ttSchemaField

/* Definitions for BROWSE brAvailable                                   */
&Scoped-define FIELDS-IN-QUERY-brAvailable ttSchemaField.cFullName   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brAvailable   
&Scoped-define SELF-NAME brAvailable
&Scoped-define OPEN-QUERY-brAvailable DO WITH FRAME {&FRAME-NAME}:   OPEN QUERY {&SELF-NAME}   FOR EACH ttSchemaField     WHERE ttSchemaField.lSelected = FALSE       AND ttSchemaField.lShow     = TRUE       AND ttSchemaField.cFullName MATCHES '*' + fiFilterAvailable:SCREEN-VALUE + '*'     BY ttSchemaField.cFullName.  END.
&Scoped-define TABLES-IN-QUERY-brAvailable ttSchemaField
&Scoped-define FIRST-TABLE-IN-QUERY-brAvailable ttSchemaField


/* Definitions for BROWSE brSelected                                    */
&Scoped-define FIELDS-IN-QUERY-brSelected ttSchemaField.iOrder ttSchemaField.cFullName ttSchemaField.cLabel ttSchemaField.cFormat   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brSelected   
&Scoped-define SELF-NAME brSelected
&Scoped-define OPEN-QUERY-brSelected DO WITH FRAME {&FRAME-NAME}:   OPEN QUERY {&SELF-NAME}   FOR EACH ttSchemaField     WHERE (    ttSchemaField.lSelected = TRUE            AND ttSchemaField.cFullName MATCHES '*' + fiFilterSelected:SCREEN-VALUE + '*')         BY ttSchemaField.iOrder. END.
&Scoped-define TABLES-IN-QUERY-brSelected ttSchemaField
&Scoped-define FIRST-TABLE-IN-QUERY-brSelected ttSchemaField


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brAvailable}~
    ~{&OPEN-QUERY-brSelected}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnBottom RECT-1 fiFilterAvailable ~
fiFilterSelected brAvailable brSelected fiLabel btnDown fiFormat btnTop ~
btnUp btnDeselect btnSelect 
&Scoped-Define DISPLAYED-OBJECTS fiFilterAvailable fiFilterSelected fiLabel ~
fiFormat 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnBottom  NO-FOCUS FLAT-BUTTON
     LABEL "Bttm" 
     SIZE 7 BY 1.67 TOOLTIP "move field to the bottom".

DEFINE BUTTON btnDeselect  NO-FOCUS FLAT-BUTTON
     LABEL "<=" 
     SIZE 7 BY 1.67.

DEFINE BUTTON btnDown  NO-FOCUS FLAT-BUTTON
     LABEL "Down" 
     SIZE 7 BY 1.67 TOOLTIP "move field down".

DEFINE BUTTON btnSelect  NO-FOCUS FLAT-BUTTON
     LABEL "=>" 
     SIZE 7 BY 1.67.

DEFINE BUTTON btnTop  NO-FOCUS FLAT-BUTTON
     LABEL "Top" 
     SIZE 7 BY 1.67 TOOLTIP "move field to the top".

DEFINE BUTTON btnUp  NO-FOCUS FLAT-BUTTON
     LABEL "Up" 
     SIZE 7 BY 1.67 TOOLTIP "move field up".

DEFINE VARIABLE fiFilterAvailable AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE fiFilterSelected AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 87 BY 1 NO-UNDO.

DEFINE VARIABLE fiFormat AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Format" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 TOOLTIP "enter the format of the field" NO-UNDO.

DEFINE VARIABLE fiLabel AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Label" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 TOOLTIP "enter the label for the field" NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37.8 BY 5.95.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brAvailable FOR 
      ttSchemaField SCROLLING.

DEFINE QUERY brSelected FOR 
      ttSchemaField SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brAvailable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brAvailable C-Win _FREEFORM
  QUERY brAvailable DISPLAY
      ttSchemaField.cFullName
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 45 BY 13.81 FIT-LAST-COLUMN.

DEFINE BROWSE brSelected
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brSelected C-Win _FREEFORM
  QUERY brSelected DISPLAY
      ttSchemaField.iOrder
ttSchemaField.cFullName WIDTH 40
ttSchemaField.cLabel WIDTH 20
ttSchemaField.cFormat WIDTH 20
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 87 BY 13.81 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnBottom AT ROW 9.95 COL 143.6 WIDGET-ID 34
     fiFilterAvailable AT ROW 1.19 COL 2 NO-LABEL WIDGET-ID 6
     fiFilterSelected AT ROW 1.19 COL 53.6 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     brAvailable AT ROW 2.24 COL 2 WIDGET-ID 200
     brSelected AT ROW 2.24 COL 55.6 WIDGET-ID 300
     fiLabel AT ROW 6.52 COL 165.4 COLON-ALIGNED WIDGET-ID 24
     btnDown AT ROW 7.95 COL 143.6 WIDGET-ID 22
     fiFormat AT ROW 7.95 COL 165.4 COLON-ALIGNED WIDGET-ID 26
     btnTop AT ROW 3.95 COL 143.6 WIDGET-ID 32
     btnUp AT ROW 5.95 COL 143.6 WIDGET-ID 18
     btnDeselect AT ROW 7.95 COL 48 WIDGET-ID 4
     btnSelect AT ROW 5.95 COL 48 WIDGET-ID 2
     "Field properties" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 4.57 COL 159 WIDGET-ID 30
     RECT-1 AT ROW 4.86 COL 157.6 WIDGET-ID 28
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 200 BY 15.5 WIDGET-ID 100.


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
         HEIGHT             = 15.67
         WIDTH              = 200
         MAX-HEIGHT         = 19.43
         MAX-WIDTH          = 200
         VIRTUAL-HEIGHT     = 19.43
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
  FOR EACH ttSchemaField
    WHERE ttSchemaField.lSelected = FALSE
      AND ttSchemaField.lShow     = TRUE
      AND ttSchemaField.cFullName MATCHES '*' + fiFilterAvailable:SCREEN-VALUE + '*'
    BY ttSchemaField.cFullName.

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
  FOR EACH ttSchemaField
    WHERE (    ttSchemaField.lSelected = TRUE
           AND ttSchemaField.cFullName MATCHES '*' + fiFilterSelected:SCREEN-VALUE + '*')

       BY ttSchemaField.iOrder.
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
  ttSchemaField.cFullName:BGCOLOR IN BROWSE {&Browse-name} = iColor.
  
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
  ttSchemaField.cFullName:BGCOLOR IN BROWSE {&Browse-name} = iColor. 
  ttSchemaField.cLabel   :BGCOLOR IN BROWSE {&Browse-name} = iColor. 
  ttSchemaField.cFormat  :BGCOLOR IN BROWSE {&Browse-name} = iColor. 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brSelected C-Win
ON VALUE-CHANGED OF brSelected IN FRAME DEFAULT-FRAME
DO:

  IF brSelected:QUERY:NUM-RESULTS > 0 THEN 
    BROWSE brSelected:FETCH-SELECTED-ROW(1).
      
  fiLabel :SCREEN-VALUE = (IF AVAILABLE ttSchemaField THEN ttSchemaField.cLabel ELSE '').
  fiFormat:SCREEN-VALUE = (IF AVAILABLE ttSchemaField THEN ttSchemaField.cFormat ELSE '').
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBottom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBottom C-Win
ON CHOOSE OF btnBottom IN FRAME DEFAULT-FRAME /* Bttm */
OR 'ctrl-shift-cursor-down' OF brSelected
OR 'ctrl-end' OF brSelected
DO:
  brSelected:FETCH-SELECTED-ROW(1).
  IF AVAILABLE ttSchemaField THEN 
    RUN moveField(ttSchemaField.cFullName, 'bottom').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeselect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeselect C-Win
ON CHOOSE OF btnDeselect IN FRAME DEFAULT-FRAME /* <= */
OR 'default-action' OF brSelected
OR 'delete' OF brSelected
DO:
  RUN selectField(brSelected:HANDLE,brAvailable:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDown C-Win
ON CHOOSE OF btnDown IN FRAME DEFAULT-FRAME /* Down */
OR 'ctrl-cursor-down' OF brSelected
DO:
  brSelected:FETCH-SELECTED-ROW(1).
  IF AVAILABLE ttSchemaField THEN 
    RUN moveField(ttSchemaField.cFullName, 'down').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelect C-Win
ON CHOOSE OF btnSelect IN FRAME DEFAULT-FRAME /* => */
OR 'default-action' OF brAvailable
OR 'insert' OF brAvailable
DO:
  RUN selectField(brAvailable:HANDLE, brSelected:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTop C-Win
ON CHOOSE OF btnTop IN FRAME DEFAULT-FRAME /* Top */
OR 'ctrl-shift-cursor-up' OF brSelected
OR 'ctrl-home' OF brSelected
DO:
  brSelected:FETCH-SELECTED-ROW(1).
  IF AVAILABLE ttSchemaField THEN 
    RUN moveField(ttSchemaField.cFullName, 'top').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUp C-Win
ON CHOOSE OF btnUp IN FRAME DEFAULT-FRAME /* Up */
OR 'ctrl-cursor-up' OF brSelected
DO:
  brSelected:FETCH-SELECTED-ROW(1).
  IF AVAILABLE ttSchemaField THEN 
    RUN moveField(ttSchemaField.cFullName, 'up').
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


&Scoped-define SELF-NAME fiLabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLabel C-Win
ON VALUE-CHANGED OF fiLabel IN FRAME DEFAULT-FRAME /* Label */
, fiFormat
DO:  
  DEFINE BUFFER bField FOR ttField.
   
  giRowNr = 0.
  BROWSE brSelected:FETCH-SELECTED-ROW(1).
  FIND bField WHERE bField.FieldName = ttSchemaField.cFullName NO-ERROR.
  
  IF AVAILABLE bField THEN 
  DO:
    ASSIGN 
      bField.fieldLabel     = fiLabel:SCREEN-VALUE
      bField.fieldFormat    = fiFormat:SCREEN-VALUE
      ttSchemaField.cLabel  = fiLabel:SCREEN-VALUE
      ttSchemaField.cFormat = fiFormat:SCREEN-VALUE
      .
    brSelected:REFRESH().
  END.
      
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
  DISPLAY fiFilterAvailable fiFilterSelected fiLabel fiFormat 
      WITH FRAME DEFAULT-FRAME.
  ENABLE btnBottom RECT-1 fiFilterAvailable fiFilterSelected brAvailable 
         brSelected fiLabel btnDown fiFormat btnTop btnUp btnDeselect btnSelect 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

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
    btnSelect  :LOAD-IMAGE('Image\right.gif').
    btnDeselect:LOAD-IMAGE('Image\left.gif').
    btnTop     :LOAD-IMAGE('Image\top.gif').
    btnUp      :LOAD-IMAGE('Image\up.gif').
    btnDown    :LOAD-IMAGE('Image\down.gif').
    btnBottom  :LOAD-IMAGE('Image\bottom.gif').

  END.
  
END PROCEDURE. /* ScreenInit */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moveField C-Win 
PROCEDURE moveField :
/* Move Field up or down
*/
  DEFINE INPUT PARAMETER pcField     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcDirection AS CHARACTER NO-UNDO.

  DEFINE VARIABLE iTemp AS INTEGER     NO-UNDO.
  DEFINE BUFFER bField1 FOR ttSchemaField.
  DEFINE BUFFER bField2 FOR ttSchemaField.
  
  /* Find org Field */
  FIND bField1 WHERE bField1.cFullName = pcField NO-ERROR.
  IF NOT AVAILABLE bField1 THEN RETURN. 
  
  CASE pcDirection:
    WHEN 'top' THEN FIND FIRST bField2 USE-INDEX idxOrder 
      WHERE bField2.lSelected = TRUE
        AND bField2.iOrder    < bField1.iOrder NO-ERROR.
  
    WHEN 'up' THEN FIND LAST bField2 USE-INDEX idxOrder 
      WHERE bField2.lSelected = TRUE
        AND bField2.iOrder    < bField1.iOrder NO-ERROR.
        
    WHEN 'down' THEN FIND FIRST bField2 USE-INDEX idxOrder 
      WHERE bField2.lSelected = TRUE
        AND bField2.iOrder    > bField1.iOrder NO-ERROR.
        
    WHEN 'bottom' THEN FIND LAST bField2 USE-INDEX idxOrder 
      WHERE bField2.lSelected = TRUE 
        AND bField2.iOrder    > bField1.iOrder NO-ERROR.
  END CASE.
  
  IF AVAILABLE bField2 THEN 
  DO:
    iTemp = bField1.iOrder.
    bField1.iOrder = bField2.iOrder.
    bField2.iOrder = iTemp.  

    RUN saveData.
    RUN reopenBrowses.
  END.
  
END PROCEDURE. /* moveField */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopenBrowses C-Win 
PROCEDURE reopenBrowses :
/* Reopen Field browses
*/
  DEFINE VARIABLE rAvailable AS ROWID NO-UNDO.
  DEFINE VARIABLE rSelected  AS ROWID NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF brAvailable:QUERY:NUM-RESULTS > 0 THEN 
    DO:
      BROWSE brAvailable:FETCH-SELECTED-ROW(1).
      IF AVAILABLE ttSchemaField THEN rAvailable = ROWID(ttSchemaField).
    END.
    
    IF brSelected:QUERY:NUM-RESULTS > 0 THEN 
    DO:
      BROWSE brSelected:FETCH-SELECTED-ROW(1).
      IF AVAILABLE ttSchemaField THEN rSelected = ROWID(ttSchemaField).
    END.

    giRowNr = 0.
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
/* save current state of Fields in the dataset
*/
  DEFINE BUFFER bField FOR ttField.
  DEFINE BUFFER bQuery FOR ttQuery.
  
  FOR EACH ttSchemaField:
  
    FIND bQuery. 
    FIND bField WHERE bField.FieldName = ttSchemaField.cFullName NO-ERROR.
    
    /* Preserve order of Fields */
    IF AVAILABLE bField THEN bField.orderNr = ttSchemaField.iOrder.
    
    /* Field got selected */
    IF ttSchemaField.lSelected = TRUE AND NOT AVAILABLE bField THEN
    DO:
      CREATE bField.
      ASSIGN
        bField.queryNr     = bQuery.queryNr
        bField.tableName   = ttSchemaField.cTableName
        bField.fieldName   = ttSchemaField.cFullName
        bField.fieldFormat = ttSchemaField.cFormat
        bField.fieldLabel  = ttSchemaField.cLabel
        bField.datatype    = ttSchemaField.cDataType
        .      
    END. /* selected */
    
    /* Field got deselected */
    IF ttSchemaField.lSelected = FALSE AND AVAILABLE bField THEN
    DO:
      DELETE bField.
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

    IF NOT CAN-FIND(FIRST ttSchemaField
                    WHERE ttSchemaField.lSelected = TRUE) THEN 
      pcError = 'Please select at least one field'.
    
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
  DEFINE BUFFER bField FOR ttField.
  DEFINE BUFFER bTable FOR ttTable.

  DO WITH FRAME {&frame-name}:
    
    /* (re)init fields */
    FOR EACH ttSchemaField:
      
      ttSchemaField.lSelected = FALSE.
      ttSchemaField.lShow     = CAN-FIND(bTable WHERE bTable.tableName = ttSchemaField.cTableName).
      
      FIND bField WHERE bField.fieldName = ttSchemaField.cFullName NO-ERROR.
      IF AVAILABLE bField THEN
      DO:
        ASSIGN 
          ttSchemaField.cFormat   = bField.fieldFormat  
          ttSchemaField.cLabel    = bField.fieldLabel  
          ttSchemaField.lSelected = TRUE
          ttSchemaField.iOrder    = bField.orderNr
          .    
      END.
    END.     
    
    RUN reopenBrowses.
    APPLY 'value-changed' TO brSelected.
    APPLY 'entry' TO brAvailable.
    
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectField C-Win 
PROCEDURE selectField :
/* Select / deselect Field
*/
  DEFINE INPUT PARAMETER phOldBrowse AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER phNewBrowse AS HANDLE NO-UNDO.

  DEFINE VARIABLE rOldRecord AS ROWID NO-UNDO.
  DEFINE VARIABLE rNewRecord AS ROWID NO-UNDO.
  DEFINE VARIABLE hOldBuffer AS HANDLE      NO-UNDO.
  
  DEFINE BUFFER bField FOR ttSchemaField. 
  
  APPLY 'entry' TO phOldBrowse.
  
  /* remember rowid of selected Field */
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
  FIND ttSchemaField WHERE ROWID(ttSchemaField) = rOldRecord NO-ERROR.
  FIND LAST bField WHERE bField.lSelected = (NOT ttSchemaField.lSelected) NO-ERROR.    
  ttSchemaField.lSelected = NOT ttSchemaField.lSelected.
  ttSchemaField.iOrder = (IF AVAILABLE bField THEN bField.iOrder + 1 ELSE 1).
  
  RUN saveData.

  {&OPEN-QUERY-brSelected}
  {&OPEN-QUERY-brAvailable}  
  
  IF rOldRecord <> ? THEN phNewBrowse:QUERY:REPOSITION-TO-ROWID(rOldRecord) NO-ERROR.
  IF rNewRecord <> ? THEN phOldBrowse:QUERY:REPOSITION-TO-ROWID(rNewRecord) NO-ERROR.
  APPLY 'VALUE-CHANGED' TO brSelected.

END PROCEDURE. /* selectField */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
