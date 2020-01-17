&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
    File        : wQueryRelations.w
    Purpose     : Modify relations of a query

    Author(s)   : Patrick Tingen
    Created     : 2019

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

{queryLib.i &reference-only=reference-only}

DEFINE INPUT  PARAMETER phParent AS HANDLE NO-UNDO.
DEFINE OUTPUT PARAMETER phFrame  AS HANDLE NO-UNDO.

DEFINE VARIABLE giLightGray AS INTEGER NO-UNDO.
DEFINE VARIABLE giRowNr     AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brTables

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttTable

/* Definitions for BROWSE brTables                                      */
&Scoped-define FIELDS-IN-QUERY-brTables ttTable.tableName ttTable.TableDesc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brTables   
&Scoped-define SELF-NAME brTables
&Scoped-define QUERY-STRING-brTables FOR EACH ttTable BY ttTable.orderNr
&Scoped-define OPEN-QUERY-brTables OPEN QUERY {&SELF-NAME} FOR EACH ttTable BY ttTable.orderNr.
&Scoped-define TABLES-IN-QUERY-brTables ttTable
&Scoped-define FIRST-TABLE-IN-QUERY-brTables ttTable


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brTables cbParentTable tgAutoJoin ~
edConditions btnTest 
&Scoped-Define DISPLAYED-OBJECTS cbParentTable tgAutoJoin edConditions 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnTest 
     LABEL "&Test" 
     SIZE 15 BY 1.14 TOOLTIP "test the query".

DEFINE VARIABLE cbParentTable AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Parent table" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE edConditions AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 76 BY 10.24 TOOLTIP "extra conditions for the relation"
     FGCOLOR 9 FONT 0 NO-UNDO.

DEFINE VARIABLE tgAutoJoin AS LOGICAL INITIAL no 
     LABEL "&Auto Join" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 TOOLTIP "select if you want to automatically join this table to its parent" NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brTables FOR 
      ttTable SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brTables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brTables C-Win _FREEFORM
  QUERY brTables DISPLAY
      ttTable.tableName
ttTable.TableDesc
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 80 BY 13.33 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brTables AT ROW 2.43 COL 2 WIDGET-ID 200
     cbParentTable AT ROW 2.43 COL 103 COLON-ALIGNED WIDGET-ID 40
     tgAutoJoin AT ROW 2.43 COL 139 WIDGET-ID 42
     edConditions AT ROW 3.86 COL 105 NO-LABEL WIDGET-ID 44
     btnTest AT ROW 14.57 COL 166 WIDGET-ID 48
     "Where:" VIEW-AS TEXT
          SIZE 7.2 BY .62 AT ROW 4.05 COL 97.4 WIDGET-ID 46
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
         TITLE              = "Query Properties"
         HEIGHT             = 15.14
         WIDTH              = 200.4
         MAX-HEIGHT         = 20.14
         MAX-WIDTH          = 204.2
         VIRTUAL-HEIGHT     = 20.14
         VIRTUAL-WIDTH      = 204.2
         SHOW-IN-TASKBAR    = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
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
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB brTables TEXT-1 DEFAULT-FRAME */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brTables
/* Query rebuild information for BROWSE brTables
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttTable BY ttTable.orderNr.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE brTables */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME brTables
&Scoped-define SELF-NAME brTables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brTables C-Win
ON ROW-DISPLAY OF brTables IN FRAME DEFAULT-FRAME
DO:
  DEFINE VARIABLE iColor AS INTEGER NO-UNDO.
  
  giRowNr = giRowNr + 1.
  iColor = (IF giRowNr MODULO 2 = 1 THEN ? ELSE giLightGray).
  ttTable.tableName:BGCOLOR IN BROWSE {&Browse-name} = iColor. 
  ttTable.tableDesc:BGCOLOR IN BROWSE {&Browse-name} = iColor. 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brTables C-Win
ON VALUE-CHANGED OF brTables IN FRAME DEFAULT-FRAME
DO:
  DEFINE BUFFER btTable FOR ttTable.
  
  /* Repopulate parent combo */
  cbParentTable:LIST-ITEMS = ' '.
  FOR EACH btTable WHERE btTable.orderNr < ttTable.orderNr:
    cbParentTable:ADD-LAST(btTable.tableName).
  END.  
  
  IF AVAILABLE ttTable THEN 
  DO:
    cbParentTable:SCREEN-VALUE = ttTable.parentTable NO-ERROR.
    edConditions:SCREEN-VALUE  = ttTable.conditions.
    tgAutoJoin:CHECKED         = ttTable.autoJoin.
    tgAutoJoin:SENSITIVE       = (ttTable.orderNr > 1).
  END.
  ELSE 
  DO:
    cbParentTable:SCREEN-VALUE = ''.
    edConditions:SCREEN-VALUE  = ''.
    tgAutoJoin:CHECKED         = NO.
    tgAutoJoin:SENSITIVE       = NO.
  END.  

  APPLY 'VALUE-CHANGED' TO cbParentTable. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTest C-Win
ON CHOOSE OF btnTest IN FRAME DEFAULT-FRAME /* Test */
DO:
  DEFINE VARIABLE cError AS CHARACTER NO-UNDO.
  
  RUN ScreenValidate(OUTPUT cError).
  
  IF cError <> '' THEN
    MESSAGE cError VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  ELSE   
    MESSAGE 'Query looks ok' VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbParentTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbParentTable C-Win
ON VALUE-CHANGED OF cbParentTable IN FRAME DEFAULT-FRAME /* Parent table */
DO:
  DEFINE VARIABLE lCanJoin AS LOGICAL NO-UNDO.
  
  /* Save input */
  IF NOT AVAILABLE ttTable THEN RETURN. 
  ttTable.parentTable = SELF:SCREEN-VALUE.
  IF ttTable.parentTable = ? THEN ttTable.parentTable = ''.

  /* No parent = no autojoin */
  IF ttTable.parentTable = '' OR ttTable.parentTable = ? THEN 
  DO:
    tgAutoJoin:SENSITIVE = FALSE.
    tgAutoJoin:CHECKED = FALSE.
    ttTable.autoJoin = FALSE.
  END.
  
  /* Create a query to find out if we can autojoin */
  lCanJoin = canHaveRelation(ttTable.parentTable, ttTable.tableName).
  
  tgAutoJoin:SENSITIVE IN FRAME {&frame-name} = lCanJoin.
  IF NOT lCanJoin THEN 
  DO:
    tgAutoJoin:CHECKED = FALSE.
    ttTable.autoJoin = FALSE.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME edConditions
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL edConditions C-Win
ON VALUE-CHANGED OF edConditions IN FRAME DEFAULT-FRAME
DO:
  IF AVAILABLE ttTable THEN ttTable.conditions = TRIM(SELF:SCREEN-VALUE,'~n ').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgAutoJoin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgAutoJoin C-Win
ON VALUE-CHANGED OF tgAutoJoin IN FRAME DEFAULT-FRAME /* Auto Join */
DO:
  IF AVAILABLE ttTable THEN ttTable.autoJoin = SELF:CHECKED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  DISPLAY cbParentTable tgAutoJoin edConditions 
      WITH FRAME DEFAULT-FRAME.
  ENABLE brTables cbParentTable tgAutoJoin edConditions btnTest 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScreenInit C-Win 
PROCEDURE ScreenInit :
/* Bind the dataset to the screen
*/
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsQuery BIND.

  giLightGray = getLightGray().

  
END PROCEDURE. /* ScreenInit */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScreenValidate C-Win 
PROCEDURE ScreenValidate :
/* Update db with info from screen
*/
  DEFINE OUTPUT PARAMETER pcError AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE hQuery AS HANDLE      NO-UNDO.
  DEFINE VARIABLE cError AS CHARACTER   NO-UNDO.
  
  DEFINE BUFFER bTable FOR ttTable.
  
  DO WITH FRAME {&frame-name}:
  
    FOR EACH bTable WHERE bTable.orderNr > 1:
    
      IF (bTable.orderNr > 1) 
      AND (bTable.parentTable = '' OR bTable.parentTable = ?) THEN
      DO:
        pcError = SUBSTITUTE('Table "&1" is not linked to a parent table'
                            , bTable.tableName).
        RETURN. 
      END.
    
      IF    bTable.orderNr    > 1
        AND bTable.autoJoin   = FALSE
        AND bTable.conditions = '' THEN
      DO:
        pcError = SUBSTITUTE('Table "&1" is not auto-joined and has no conditions'
                            , bTable.tableName).
        RETURN. 
      END.

      /* Test the query up to this point */
      RUN openQuery
        ( INPUT DATASET dsQuery BY-REFERENCE
        , INPUT bTable.orderNr
        , INPUT FALSE
        , OUTPUT hQuery
        , OUTPUT pcError
        ).
      RUN closeQuery(hQuery).
      
      IF pcError <> '' THEN
      DO:
        pcError = SUBSTITUTE('Error while adding table "&1" to the query: ~n~n&2', bTable.tableName, pcError).
        RETURN. 
      END.    
      
    END. /* for each bTable */  
  END.

END PROCEDURE. /* ScreenValidate */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScreenHide C-Win 
PROCEDURE ScreenHide :
/* Update db with info from screen
*/
  DEFINE BUFFER bQuery FOR ttQuery.

  DO WITH FRAME {&frame-name}:
  
    
  END.

END PROCEDURE. /* ScreenHide */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScreenShow C-Win 
PROCEDURE ScreenShow :
/* Get latest info from db and show
*/
  DEFINE BUFFER bQuery FOR ttQuery.

  DO WITH FRAME {&frame-name}:
    {&OPEN-QUERY-brTables}
    APPLY 'value-changed' TO brTables IN FRAME {&frame-name}.
    APPLY 'entry' TO brTables.
  END.
  
END PROCEDURE. /* ScreenShow */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

