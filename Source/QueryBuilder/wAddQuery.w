&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          qb               PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------
    File        : wAddQuery.w
    Purpose     : Show list of predefined queries and let user choose one

    Author(s)   : Patrick Tingen
    Created     : 2019

  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_IS_RUNNING) = 0 &THEN
  DEFINE OUTPUT PARAMETER piQueryNr AS INTEGER NO-UNDO.  
&ELSE
  DEFINE VARIABLE piQueryNr AS INTEGER NO-UNDO.
&ENDIF

{ queryLib.i }

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brQueries

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES qbQuery

/* Definitions for BROWSE brQueries                                     */
&Scoped-define FIELDS-IN-QUERY-brQueries qbQuery.queryName 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brQueries 
&Scoped-define QUERY-STRING-brQueries FOR EACH qbQuery ~
      WHERE qbQuery.predefined = TRUE NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brQueries OPEN QUERY brQueries FOR EACH qbQuery ~
      WHERE qbQuery.predefined = TRUE NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brQueries qbQuery
&Scoped-define FIRST-TABLE-IN-QUERY-brQueries qbQuery


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brQueries}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brQueries BtnCancel 
&Scoped-Define DISPLAYED-OBJECTS edQueryDesc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAdd AUTO-GO 
     LABEL "Add to my queries" 
     SIZE 23 BY 1.14 TOOLTIP "add the selected query to your list".

DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE edQueryDesc AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 49 BY 10.71 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brQueries FOR 
      qbQuery SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brQueries
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brQueries C-Win _STRUCTURED
  QUERY brQueries NO-LOCK DISPLAY
      qbQuery.queryName FORMAT "x(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 60 BY 12.62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brQueries AT ROW 1.48 COL 3 WIDGET-ID 200
     edQueryDesc AT ROW 3.38 COL 64 HELP
          "longer description of the query" NO-LABEL WIDGET-ID 2
     btnAdd AT ROW 14.81 COL 40 WIDGET-ID 12
     BtnCancel AT ROW 14.81 COL 64 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 115.2 BY 15.62
         DEFAULT-BUTTON btnAdd CANCEL-BUTTON BtnCancel WIDGET-ID 100.


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
         TITLE              = "Add New Query"
         HEIGHT             = 16
         WIDTH              = 116.4
         MAX-HEIGHT         = 29.57
         MAX-WIDTH          = 161.6
         VIRTUAL-HEIGHT     = 29.57
         VIRTUAL-WIDTH      = 161.6
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB brQueries 1 DEFAULT-FRAME */
/* SETTINGS FOR BUTTON btnAdd IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR edQueryDesc IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       edQueryDesc:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brQueries
/* Query rebuild information for BROWSE brQueries
     _TblList          = "qb.qbQuery"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "qb.qbQuery.predefined = TRUE"
     _FldNameList[1]   = qb.qbQuery.queryName
     _Query            is OPENED
*/  /* BROWSE brQueries */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Add New Query */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Add New Query */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME DEFAULT-FRAME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DEFAULT-FRAME C-Win
ON GO OF FRAME DEFAULT-FRAME
OR 'default-action' OF brQueries
DO:

  piQueryNr = (IF AVAILABLE qbQuery THEN qbQuery.queryNr ELSE 0).   
  APPLY 'close' TO THIS-PROCEDURE.   
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brQueries
&Scoped-define SELF-NAME brQueries
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brQueries C-Win
ON VALUE-CHANGED OF brQueries IN FRAME DEFAULT-FRAME
DO:

  edQueryDesc:SCREEN-VALUE = (IF AVAILABLE qbQuery THEN qbQuery.queryDesc ELSE '').
  btnAdd:SENSITIVE = (AVAILABLE qbQuery).
  
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
  RUN initObject.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
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
  DISPLAY edQueryDesc 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE brQueries BtnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initObject C-Win 
PROCEDURE initObject :
/* Init & startup
*/
  DO WITH FRAME {&Frame-name}:
  
    APPLY 'value-changed' TO brQueries.
    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
