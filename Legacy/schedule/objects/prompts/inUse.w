&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ID.w

  Description: Identifier

  Input Parameters: <none>

  Output Parameters: ID

  Author: Ron Stark

  Created: 5.12.2004

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

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT PARAMETER ipID AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE ipID AS CHARACTER NO-UNDO INITIAL 'MXP/SO'.
&ENDIF

/* Local Variable Definitions ---                                       */

{schedule/scopDir.i}

DEFINE VARIABLE inUseList AS CHARACTER NO-UNDO.

SESSION:SET-WAIT-STATE('').

{{&includes}/findProgram.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IDList btnRemove btnContinue 
&Scoped-Define DISPLAYED-OBJECTS IDList 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnContinue 
     LABEL "&Continue" 
     SIZE 12 BY 1.

DEFINE BUTTON btnRemove 
     LABEL "&Remove" 
     SIZE 12.2 BY 1.

DEFINE VARIABLE IDList AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "","" 
     SIZE 55 BY 7.62
     FONT 2 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     IDList AT ROW 1.71 COL 1 NO-LABEL
     btnRemove AT ROW 9.57 COL 30
     btnContinue AT ROW 9.57 COL 43
     " Date         Time           User" VIEW-AS TEXT
          SIZE 51 BY .62 AT ROW 1 COL 1
          FONT 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 55.4 BY 9.57.


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
         TITLE              = "In Use List"
         HEIGHT             = 9.57
         WIDTH              = 55.4
         MAX-HEIGHT         = 12
         MAX-WIDTH          = 67.6
         VIRTUAL-HEIGHT     = 12
         VIRTUAL-WIDTH      = 67.6
         SMALL-TITLE        = yes
         SHOW-IN-TASKBAR    = no
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         ALWAYS-ON-TOP      = yes
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* In Use List */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* In Use List */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnContinue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnContinue C-Win
ON CHOOSE OF btnContinue IN FRAME DEFAULT-FRAME /* Continue */
DO:
  APPLY 'CLOSE':U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRemove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemove C-Win
ON CHOOSE OF btnRemove IN FRAME DEFAULT-FRAME /* Remove */
DO:
  IF IDList:SCREEN-VALUE EQ '' THEN RETURN NO-APPLY.
  MESSAGE 'Remove "In Use File: ' + IDList:SCREEN-VALUE + '"?' VIEW-AS ALERT-BOX
    QUESTION BUTTONS YES-NO UPDATE removeFile AS LOGICAL.
  IF removeFile THEN
  DO:
    OS-DELETE VALUE(IDList:SCREEN-VALUE).
    RUN showInUseFiles (ipID,OUTPUT inUseList).
  END.
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
  RUN showInUseFiles (ipID,OUTPUT inUseList).
  RUN enable_UI.
  IF inUseList EQ '' THEN RETURN.
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
  DISPLAY IDList 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE IDList btnRemove btnContinue 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getInUseFiles C-Win 
PROCEDURE getInUseFiles :
/*------------------------------------------------------------------------------
  Purpose:     get installation identifier dir names
  Parameters:  starting directory, return directory name
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipSearchDir AS CHARACTER NO-UNDO.

  DEFINE OUTPUT PARAMETER opListItems AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE fileName AS CHARACTER FORMAT 'X(40)' NO-UNDO.
  DEFINE VARIABLE attrList AS CHARACTER FORMAT 'X(4)' NO-UNDO.

  IF INDEX(PROPATH,'schedule') NE 0 AND
     INDEX(PROPATH,':') EQ 0 THEN ipSearchDir = '../' + ipSearchDir.
  INPUT FROM OS-DIR(ipSearchDir) NO-ECHO.
  REPEAT:
    SET fileName ^ attrList.
    IF attrList NE 'f' OR INDEX(fileName,'inUse') EQ 0 THEN
    NEXT.
    opListItems = IF opListItems EQ '' THEN fileName
                  ELSE opListItems + ',' +  fileName.
  END.
  INPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showInUseFiles C-Win 
PROCEDURE showInUseFiles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipID AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opInUseList AS CHARACTER NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE inUseFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE inUseDate AS CHARACTER NO-UNDO.
  DEFINE VARIABLE inUseTime AS CHARACTER NO-UNDO.
  DEFINE VARIABLE inUseUser AS CHARACTER NO-UNDO.
  DEFINE VARIABLE startDir AS CHARACTER NO-UNDO.
  
  ASSIGN
    startDir = clientDat + '{&data}/' + ipID
    IDList:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = ?.
  RUN getInUseFiles (startDir,OUTPUT opInUseList).
  IF opInUseList NE '' THEN
  DO i = 1 TO NUM-ENTRIES(opInUseList):
    inUseFile = startDir + '/' + ENTRY(i,opInUseList).
    INPUT FROM VALUE(inUseFile) NO-ECHO.
    IMPORT inUseDate inUseTime inUseUser.
    IDList:ADD-LAST(inUseDate + ' @ ' + inUseTime + ' by ' + inUseUser,inUseFile).
    INPUT CLOSE.
  END. /* do i */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

