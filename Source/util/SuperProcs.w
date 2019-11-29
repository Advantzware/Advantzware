&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: runningProcs.w

  Description: Access Currently Running Procedures/Functions

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 11.27.2019

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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/defines/sortByDefs.i}
{AOA/tempTable/ttSuperProc.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttSuperProc

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 ttSuperProc.procName ttSuperProc.internalProc ttSuperProc.procType ttSuperProc.returnType ttSuperProc.procParams   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH ttSuperProc WHERE (matchesValue EQ NO AND (ttSuperProc.procName BEGINS searchValue OR ttSuperProc.internalProc BEGINS searchValue)) OR (matchesValue EQ YES AND (ttSuperProc.procName MATCHES "*" + searchValue + "*" OR ttSuperProc.internalProc MATCHES "*" + searchValue + "*"))  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH ttSuperProc WHERE (matchesValue EQ NO AND (ttSuperProc.procName BEGINS searchValue OR ttSuperProc.internalProc BEGINS searchValue)) OR (matchesValue EQ YES AND (ttSuperProc.procName MATCHES "*" + searchValue + "*" OR ttSuperProc.internalProc MATCHES "*" + searchValue + "*"))  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 ttSuperProc
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 ttSuperProc


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS searchValue matchesValue BROWSE-2 
&Scoped-Define DISPLAYED-OBJECTS searchValue matchesValue 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 RECT-TABLE searchValue matchesValue 
&Scoped-define List-2 RECT-TABLE searchValue matchesValue 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClose 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Close" 
     SIZE 8 BY 1.91 TOOLTIP "Close"
     BGCOLOR 8 .

DEFINE BUTTON btnCopyCode AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/element_copy.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Copy Code" 
     SIZE 8 BY 1.91 TOOLTIP "Copy Code"
     BGCOLOR 8 .

DEFINE BUTTON btnCopyDefs AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/element_copy.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Copy Define" 
     SIZE 8 BY 1.91 TOOLTIP "Copy Define"
     BGCOLOR 8 .

DEFINE VARIABLE codePhrase AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 50 BY 14.76
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE definePhrase AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 70 BY 14.76
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE showDataType AS LOGICAL INITIAL yes 
     LABEL "Show Data Type" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE showPrefix AS LOGICAL INITIAL yes 
     LABEL "Show ip/op Prefix" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE searchValue AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 138 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-TABLE
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 160 BY 1.43.

DEFINE VARIABLE matchesValue AS LOGICAL INITIAL yes 
     LABEL "Matches" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      ttSuperProc SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 C-Win _FREEFORM
  QUERY BROWSE-2 DISPLAY
      ttSuperProc.procName LABEL-BGCOLOR 14
ttSuperProc.internalProc LABEL-BGCOLOR 14
ttSuperProc.procType LABEL-BGCOLOR 14
ttSuperProc.returnType
ttSuperProc.procParams
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 160 BY 8.1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     searchValue AT ROW 1.24 COL 2 WIDGET-ID 62
     matchesValue AT ROW 1.24 COL 149 HELP
          "Select for Table Search Matches" WIDGET-ID 40
     BROWSE-2 AT ROW 2.43 COL 1 WIDGET-ID 200
     RECT-TABLE AT ROW 1 COL 1 WIDGET-ID 46
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.62
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.

DEFINE FRAME codeView
     btnCopyDefs AT ROW 16.24 COL 2 WIDGET-ID 16
     btnCopyCode AT ROW 16.24 COL 73 WIDGET-ID 6
     definePhrase AT ROW 1.24 COL 2 NO-LABEL WIDGET-ID 14
     btnClose AT ROW 16.24 COL 115 WIDGET-ID 4
     codePhrase AT ROW 1.24 COL 73 NO-LABEL WIDGET-ID 2
     showPrefix AT ROW 16.71 COL 30 HELP
          "Show ip/op Prefix?" WIDGET-ID 12
     showDataType AT ROW 16.71 COL 88 HELP
          "Show Data Type?" WIDGET-ID 10
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 38 ROW 11.24
         SIZE 123 BY 18.33
         FGCOLOR 1 
         TITLE "Code View" WIDGET-ID 300.


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
         TITLE              = "Super Procedures / Functions"
         HEIGHT             = 28.57
         WIDTH              = 160
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME codeView:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME codeView
                                                                        */
ASSIGN 
       FRAME codeView:MOVABLE          = TRUE.

ASSIGN 
       codePhrase:READ-ONLY IN FRAME codeView        = TRUE.

ASSIGN 
       definePhrase:READ-ONLY IN FRAME codeView        = TRUE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 matchesValue DEFAULT-FRAME */
ASSIGN 
       BROWSE-2:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR TOGGLE-BOX matchesValue IN FRAME DEFAULT-FRAME
   1 2                                                                  */
ASSIGN 
       matchesValue:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-TABLE IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 2                                                        */
ASSIGN 
       RECT-TABLE:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN searchValue IN FRAME DEFAULT-FRAME
   ALIGN-L 1 2                                                          */
ASSIGN 
       searchValue:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSuperProc
WHERE (matchesValue EQ NO
AND (ttSuperProc.procName BEGINS searchValue
OR ttSuperProc.internalProc BEGINS searchValue))
OR (matchesValue EQ YES
AND (ttSuperProc.procName MATCHES "*" + searchValue + "*"
OR ttSuperProc.internalProc MATCHES "*" + searchValue + "*"))
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME codeView
/* Query rebuild information for FRAME codeView
     _Query            is NOT OPENED
*/  /* FRAME codeView */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Super Procedures / Functions */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Super Procedures / Functions */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Super Procedures / Functions */
DO:
    RUN pWinResize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 C-Win
ON DEFAULT-ACTION OF BROWSE-2 IN FRAME DEFAULT-FRAME
DO:
    VIEW FRAME codeView.
    APPLY "VALUE-CHANGED":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 C-Win
ON START-SEARCH OF BROWSE-2 IN FRAME DEFAULT-FRAME
DO:
    IF SELF:CURRENT-COLUMN:NAME NE ? THEN DO:
        cColumnLabel = SELF:CURRENT-COLUMN:NAME.
        IF cColumnLabel EQ cSaveLabel THEN
        lAscending = NOT lAscending.
        cSaveLabel = cColumnLabel.
        RUN pReopenBrowse.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 C-Win
ON VALUE-CHANGED OF BROWSE-2 IN FRAME DEFAULT-FRAME
DO:
    RUN pCodeView.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME codeView
&Scoped-define SELF-NAME btnClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClose C-Win
ON CHOOSE OF btnClose IN FRAME codeView /* Close */
DO:
    HIDE FRAME codeView.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCopyCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopyCode C-Win
ON CHOOSE OF btnCopyCode IN FRAME codeView /* Copy Code */
DO:
    codePhrase:SET-SELECTION (1, 9999).
    codePhrase:EDIT-COPY ().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCopyDefs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopyDefs C-Win
ON CHOOSE OF btnCopyDefs IN FRAME codeView /* Copy Define */
DO:
    definePhrase:SET-SELECTION (1, 9999).
    definePhrase:EDIT-COPY ().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME matchesValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL matchesValue C-Win
ON VALUE-CHANGED OF matchesValue IN FRAME DEFAULT-FRAME /* Matches */
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME searchValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL searchValue C-Win
ON VALUE-CHANGED OF searchValue IN FRAME DEFAULT-FRAME /* Search */
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME codeView
&Scoped-define SELF-NAME showDataType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL showDataType C-Win
ON VALUE-CHANGED OF showDataType IN FRAME codeView /* Show Data Type */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pCodeView.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME showPrefix
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL showPrefix C-Win
ON VALUE-CHANGED OF showPrefix IN FRAME codeView /* Show ip/op Prefix */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pCodeView.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
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
  RUN pSuperProcs.
  RUN pWinResize.
  RUN enable_UI.
  HIDE FRAME codeView.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{methods/sortByProc.i "pByProcName" "ttSuperProc.procName"}
{methods/sortByProc.i "pByInternalProc" "ttSuperProc.internalProc"}
{methods/sortByProc.i "pByProcType" "ttSuperProc.procType"}

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
  DISPLAY searchValue matchesValue 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE searchValue matchesValue BROWSE-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY definePhrase codePhrase showPrefix showDataType 
      WITH FRAME codeView IN WINDOW C-Win.
  ENABLE btnCopyDefs btnCopyCode definePhrase btnClose codePhrase showPrefix 
         showDataType 
      WITH FRAME codeView IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-codeView}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCodeView C-Win 
PROCEDURE pCodeView :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cCodePhrase   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDataType     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDefinePhrase AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cProcField    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx           AS INTEGER   NO-UNDO.

    IF FRAME codeView:SENSITIVE THEN DO:
        ASSIGN
            cDefinePhrase = ""
            cCodePhrase   = (IF ttSuperProc.procType EQ "Procedure" THEN "RUN "
                             ELSE "DYNAMIC-FUNCTION (~"")
                          + ttSuperProc.internalProc
                          + (IF ttSuperProc.procType EQ "Function" THEN "~"" ELSE "")
                          .
        IF ttSuperProc.procParams NE "" THEN
        cCodePhrase = cCodePhrase
                    + (IF ttSuperProc.procType EQ "Procedure" THEN " (" ELSE ",")
                    + CHR(10)
                    + "    "
                    .            
        IF ttSuperProc.procParams NE "" THEN
        DO idx = 1 TO NUM-ENTRIES(ttSuperProc.procParams):
            ASSIGN
                cProcField  = ENTRY(idx,ttSuperProc.procParams)
                cDataType   = ENTRY(NUM-ENTRIES(cProcField," "),cProcField," ")
                cProcField  = REPLACE(cProcField," " + cDataType,"")
                .
            IF showPrefix EQ NO THEN
                IF cProcField BEGINS "ip" THEN
                cProcField = SUBSTRING(cProcField,3).
                ELSE
                IF cProcField BEGINS "OUTPUT op" THEN
                cProcField = REPLACE(cProcField,"OUTPUT op","OUTPUT ").
            ASSIGN
                cDefinePhrase = cDefinePhrase
                              + "DEFINE VARIABLE " + REPLACE(cProcField,"OUTPUT ","")
                              + " AS " + CAPS(cDataType) + " NO-UNDO."
                              + CHR(10)
                cCodePhrase   = cCodePhrase
                              + cProcField
                              + (IF idx NE NUM-ENTRIES(ttSuperProc.procParams) THEN ","
                                 ELSE "")
                              + (IF showDataType THEN " /* " + cDataType + " */" ELSE "")
                              + CHR(10)
                              + "    "
                              .
        END. /* do idx */
        IF ttSuperProc.procType   EQ "Function" OR
           ttSuperProc.procParams NE "" THEN
        cCodePhrase = cCodePhrase + ")".
        ASSIGN
            cCodePhrase = cCodePhrase + "."
            codePhrase:SCREEN-VALUE   = cCodePhrase
            definePhrase:SCREEN-VALUE = cDefinePhrase
            .
    END. /* if frame */

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
        WHEN "internalProc" THEN
        RUN pByInternalProc.
        WHEN "procName" THEN
        RUN pByProcName.
        WHEN "procType" THEN
        RUN pByProcType.
    END CASE.
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSuperProcs C-Win 
PROCEDURE pSuperProcs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {AOA/includes/pCreateTtSuperProcs.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinResize C-Win 
PROCEDURE pWinResize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    SESSION:SET-WAIT-STATE("General").
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            FRAME codeView:COL = 1
            FRAME codeView:ROW = 1
            .
        HIDE FRAME codeView.
        HIDE FRAME {&FRAME-NAME}.
        IF {&WINDOW-NAME}:HEIGHT LT 28.57 THEN
        {&WINDOW-NAME}:HEIGHT = 28.57.
        IF {&WINDOW-NAME}:WIDTH  LT 160   THEN
        {&WINDOW-NAME}:WIDTH  = 160.
        ASSIGN
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH
            FRAME {&FRAME-NAME}:HEIGHT         = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:WIDTH          = {&WINDOW-NAME}:WIDTH
            BROWSE {&BROWSE-NAME}:HEIGHT       = FRAME {&FRAME-NAME}:HEIGHT
                                               - BROWSE {&BROWSE-NAME}:ROW
            BROWSE {&BROWSE-NAME}:WIDTH        = FRAME {&FRAME-NAME}:WIDTH
            FRAME codeView:COL                 = FRAME {&FRAME-NAME}:WIDTH
                                               - FRAME codeView:WIDTH + 1
            FRAME codeView:ROW                 = FRAME {&FRAME-NAME}:HEIGHT
                                               - FRAME codeView:HEIGHT
            .
        VIEW FRAME {&FRAME-NAME}.
    END. /* do with */
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

