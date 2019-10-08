/* brwsbody.i */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

&Scoped-define WINDOW-NAME {&IAMWHAT}-WINDOW

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}

&IF "{&IAMWHAT}" = "SEARCH" &THEN
DEFINE OUTPUT PARAMETER save-rowid AS ROWID NO-UNDO.
&ELSE
DEFINE VARIABLE save-rowid AS ROWID NO-UNDO.
&ENDIF
DEFINE VARIABLE find-auto AS LOGICAL NO-UNDO.
DEFINE BUFFER b-prgrms FOR prgrms.

{&def-include}

&Scoped-define ENHANCE yes

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME {&IAMWHAT}-FRAME
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES {&{&IAMWHAT}-file}

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE {&where-statement}

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-{&BROWSE-NAME} {&show-fields} 
&Scoped-define TABLES-IN-QUERY-{&BROWSE-NAME} {&{&IAMWHAT}-file}
&Scoped-define FIRST-TABLE-IN-QUERY-{&BROWSE-NAME} {&{&IAMWHAT}-file}
&Scoped-define OPEN-QUERY-{&BROWSE-NAME} OPEN QUERY {&BROWSE-NAME} ~
FOR EACH {&{&IAMWHAT}-file} WHERE ~{&KEY-PHRASE} NO-LOCK ~{&SORTBY-PHRASE}.

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME} ~
    ~{&OPEN-QUERY-{&BROWSE-NAME}}

/* Standard List Definitions                                            */
&IF "{&IAMWHAT}" = "SEARCH" &THEN
&Scoped-Define ENABLED-OBJECTS {&BROWSE-NAME} RECT-1 browse-order auto_find ~
word_search Btn_Clear_Find Btn_Clear_Search Btn_Cancel Btn_OK
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find word_search
&ENDIF
&IF "{&IAMWHAT}" = "LOOKUP" &THEN
&Scoped-Define ENABLED-OBJECTS {&BROWSE-NAME} RECT-1 browse-order auto_find ~
Btn_Clear_Find Btn_Cancel Btn_OK
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 
&ENDIF

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR {&WINDOW-NAME} AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&Cancel" 
     SIZE 8 BY 1.91
     FONT 4.

DEFINE BUTTON Btn_Clear_Find 
     IMAGE-UP FILE "Graphics/32x32/close.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Clear &Find" 
     SIZE 8 BY 1.91
     FONT 4.

DEFINE BUTTON Btn_OK 
     IMAGE-UP FILE "Graphics/32x32/checkbox.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&OK" 
     SIZE 8 BY 1.91
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE {&width-size} BY 1
     BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS "N/A", 1
     SIZE {&browse-order-width} BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE {&width-size} BY 1.5.

&IF "{&IAMWHAT}" = "SEARCH" &THEN
DEFINE VARIABLE word_search AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE {&width-size} BY 1 NO-UNDO.

DEFINE BUTTON Btn_Clear_Search 
     IMAGE-UP FILE "Graphics/32x32/error.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Clear &Search" 
     SIZE 8 BY 1.91
     FONT 4.
&ENDIF

/* Query definitions                                                    */
DEFINE QUERY {&BROWSE-NAME} FOR {&{&IAMWHAT}-file} SCROLLING.

&IF DEFINED(yellowColumnsName) NE 0 &THEN
&SCOPED-DEFINE show-fields {&show-fields-yellow}
&ENDIF

/* Browse definitions                                                   */
DEFINE BROWSE {&BROWSE-NAME}
  QUERY {&BROWSE-NAME} NO-LOCK DISPLAY
      {&show-fields}
    WITH NO-ASSIGN SEPARATORS SIZE {&width-size} BY {&height-size} FONT {&font}.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME {&FRAME-NAME}
     {&BROWSE-NAME} AT ROW 1 COL 1 HELP
          "Select Record"
     browse-order AT ROW {&browse-order-row} COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW {&auto-find-row} COL 1 HELP
          "Enter Auto Find Value" NO-LABEL
&IF "{&IAMWHAT}" = "SEARCH" &THEN
     "Word Search Text ..." VIEW-AS TEXT
          SIZE 22 BY 1 AT ROW {&search-text-row} COL 1
     Btn_Clear_Search AT ROW {&search-text-row} COL {&btn-search-col} HELP
          "CLEAR SEARCH Value"
     word_search AT ROW {&word-search-row} COL 1 HELP
          "Enter Word Search Text" NO-LABEL
&ENDIF
     Btn_Clear_Find AT ROW {&btn-row} COL 1 HELP
          "CLEAR AUTO FIND Value"
     Btn_Cancel AT ROW {&btn-row} COL {&btn-cancel-col} HELP
          "Cancel Search Selection"
     Btn_OK AT ROW {&btn-row} COL {&btn-ok-col} HELP
          "Select Highlighted Record"
     RECT-1 AT ROW {&rect-1-row} COL 1
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW {&by-row} COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0
         SIZE {&width-size} BY {&window-size}.

/* *********************** Procedure Settings ************************ */

/* *************************  Create Window  ************************** */

IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW {&WINDOW-NAME} ASSIGN
         HIDDEN             = YES
         TITLE              = "{&frame-title}"
         HEIGHT             = {&window-size}
         WIDTH              = {&width-size}
         MAX-HEIGHT         = {&window-size}
         MAX-WIDTH          = {&width-size}
         VIRTUAL-HEIGHT     = {&window-size}
         VIRTUAL-WIDTH      = {&width-size}
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

&IF "{&IAMWHAT}" = "SEARCH" &THEN
&Scoped-define LOADIMAGE document_view
&ELSEIF "{&IAMWHAT}" = "LOOKUP" &THEN
&Scoped-define LOADIMAGE question
&ENDIF

IF NOT {&WINDOW-NAME}:LOAD-ICON("Graphics\32x32\{&LOADIMAGE}":U) THEN
    MESSAGE "Unable to load icon: Graphics\32x32\{&LOADIMAGE}"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.

/* ***************  Runtime Attributes and UIB Settings  ************** */

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE({&WINDOW-NAME})
THEN {&WINDOW-NAME}:HIDDEN = no.

&IF DEFINED(yellowColumnsName) NE 0 &THEN
{&BROWSE-NAME}:ALLOW-COLUMN-SEARCHING IN FRAME {&FRAME-NAME} = TRUE.
&ENDIF
/* ************************* Included-Libraries *********************** */

{methods/template/browser.i}

/* ************************  Control Triggers  ************************ */

ON END-ERROR OF {&WINDOW-NAME} OR
   ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

ON WINDOW-CLOSE OF {&WINDOW-NAME}
DO:
  /* This event will close the window and terminate the procedure.  */
  DO TRANSACTION:
     RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
  END.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* Don't run F1 from Lookup
ON HELP OF {&BROWSE-NAME} IN FRAME {&FRAME-NAME}
DO:
  &IF "{&ui-prgmname}" NE "" &THEN
  RUN Get_Procedure IN Persistent-Handle ("{&ui-prgmname}",OUTPUT run-proc,yes).
  &ENDIF
END. */

ON CHOOSE OF Btn_Cancel IN FRAME {&FRAME-NAME} /* Cancel */
DO:
  &IF "{&IAMWHAT}" = "SEARCH" &THEN
  save-rowid = ?.
  &ENDIF
  
  DO TRANSACTION:
     RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
  END.
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

ON CHOOSE OF Btn_OK IN FRAME {&FRAME-NAME} /* OK */ OR
   DEFAULT-ACTION OF {&BROWSE-NAME} IN FRAME {&FRAME-NAME}
DO:
  &IF "{&IAMWHAT}" = "SEARCH" &THEN
  save-rowid = ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
  &ELSEIF "{&IAMWHAT}" = "LOOKUP" &THEN
  ASSIGN
    g_lookup-var = STRING({&{&IAMWHAT}-file}.{&return-field})
    &IF "{&{&IAMWHAT}-db}" NE "dictdb." AND "{&{&IAMWHAT}-db}" NE " " &THEN
    FRAME-VALUE = STRING({&{&IAMWHAT}-file}.{&return-field}) NO-ERROR.
    &ELSE
    m-{&IAMWHAT}-var = STRING({&{&IAMWHAT}-file}.{&return-field}).
    &ENDIF
  &ENDIF
  
  DO TRANSACTION:
     RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
  END.

  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

&IF "{&IAMWHAT}" = "SEARCH" &THEN
&Scoped-define SELF-NAME word_search
ON LEAVE OF {&SELF-NAME} IN FRAME {&FRAME-NAME} /* Word Search */
DO:
  ASSIGN {&SELF-NAME}.
  IF {&SELF-NAME} NE "" THEN
  ENABLE Btn_Clear_Search WITH FRAME {&FRAME-NAME}.
  ELSE
  DISABLE Btn_Clear_Search WITH FRAME {&FRAME-NAME}.
  RUN Change-Order (browse-order:SCREEN-VALUE).
END.

ON RETURN OF {&SELF-NAME} IN FRAME {&FRAME-NAME} /* Word Search */
DO:
  APPLY "LEAVE" TO {&SELF-NAME}.
END.

&Scoped-define SELF-NAME Btn_Clear_Search
ON CHOOSE OF {&SELF-NAME} IN FRAME {&FRAME-NAME} /* Clear Search */
DO:
  APPLY LASTKEY.
  ASSIGN
    word_search = ""
    word_search:SCREEN-VALUE = "".
  DISABLE {&SELF-NAME} WITH FRAME {&FRAME-NAME}.
  RUN Change-Order (browse-order:SCREEN-VALUE).
END.
&ENDIF

&IF DEFINED(yellowColumnsName) NE 0 &THEN
ON START-SEARCH OF {&BROWSE-NAME} IN FRAME {&FRAME-NAME}
DO:
  RUN startSearch.
END.
&ENDIF

/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE
DO:
  {&end-include}
  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {&top-include}

  {custom/usrprint.i}

  &IF "{&IAMWHAT}" = "LOOKUP" AND "{&{&IAMWHAT}-file}" NE "convfact" &THEN
  IF (FRAME-VALUE) NE "" THEN DO:
    &IF "{&DATATYP1}" = ""   &then
      FOR EACH {&{&IAMWHAT}-db}{&{&IAMWHAT}-file} WHERE {&where-statement} AND
      {&{&IAMWHAT}-db}{&{&IAMWHAT}-file}.{&return-field} BEGINS {&DATATYP1}(FRAME-VALUE)
        NO-LOCK {&SORTBY-1}:
        LEAVE.
      END.
    &endif
    IF NOT AVAIL {&{&IAMWHAT}-db}{&{&IAMWHAT}-file} THEN
    FOR EACH {&{&IAMWHAT}-db}{&{&IAMWHAT}-file} WHERE {&where-statement} AND
    {&{&IAMWHAT}-db}{&{&IAMWHAT}-file}.{&return-field} GE {&DATATYP1}(FRAME-VALUE)
        NO-LOCK {&SORTBY-1}:
      LEAVE.
    END.
  END.
  IF NOT AVAIL {&{&IAMWHAT}-db}{&{&IAMWHAT}-file} THEN
  FOR EACH {&{&IAMWHAT}-db}{&{&IAMWHAT}-file} WHERE {&where-statement}
      NO-LOCK {&SORTBY-1}:
    LEAVE.
  END.
  save-rowid = ROWID({&{&IAMWHAT}-db}{&{&IAMWHAT}-file}).
  &ENDIF
  
  RUN enable_UI.

  DO TRANSACTION:
     RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
  END.

  &IF "{&IAMWHAT}" = "LOOKUP" AND "{&{&IAMWHAT}-file}" NE "convfact" &THEN
  DO WITH FRAME {&FRAME-NAME}:
    {&browse-name}:SET-REPOSITIONED-ROW(INT({&browse-name}:DOWN / 2),"always").
    RUN Change-Order (browse-order:SCREEN-VALUE).
  END.
  IF save-rowid NE ? THEN
  REPOSITION {&BROWSE-NAME} TO ROWID save-rowid.
  &ENDIF
  
  {methods/nowait.i}

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* **********************  Internal Procedures  *********************** */

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
  HIDE FRAME {&FRAME-NAME}.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

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
  DISPLAY {&DISPLAYED-OBJECTS}
      WITH FRAME {&FRAME-NAME} IN WINDOW {&WINDOW-NAME}.
  ENABLE {&ENABLED-OBJECTS}
      WITH FRAME {&FRAME-NAME} IN WINDOW {&WINDOW-NAME}.
  {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
  VIEW {&WINDOW-NAME}.
END PROCEDURE.
