&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: sysCtrlU.w

  Description: Show User sys-ctrl Usage

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 7.7.2018

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

{system/ttSysCtrlUsage.i}

DEFINE TEMP-TABLE bttSysCtrlUsage NO-UNDO LIKE ttSysCtrlUsage.

SESSION:SET-WAIT-STATE ("").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME sysCtrlUsage

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttSysCtrlUsage

/* Definitions for BROWSE sysCtrlUsage                                  */
&Scoped-define FIELDS-IN-QUERY-sysCtrlUsage ttSysCtrlUsage   
&Scoped-define ENABLED-FIELDS-IN-QUERY-sysCtrlUsage   
&Scoped-define SELF-NAME sysCtrlUsage
&Scoped-define QUERY-STRING-sysCtrlUsage FOR EACH ttSysCtrlUsage
&Scoped-define OPEN-QUERY-sysCtrlUsage OPEN QUERY {&SELF-NAME} FOR EACH ttSysCtrlUsage.
&Scoped-define TABLES-IN-QUERY-sysCtrlUsage ttSysCtrlUsage
&Scoped-define FIRST-TABLE-IN-QUERY-sysCtrlUsage ttSysCtrlUsage


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-sysCtrlUsage}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnClearSysCtrlUsage btnStackTrace ~
sysCtrlUsage 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClearSysCtrlUsage 
     LABEL "Clear SysCtrl Usage" 
     SIZE 24 BY 1.

DEFINE BUTTON btnStackTrace 
     LABEL "View Stack Trace" 
     SIZE 24 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY sysCtrlUsage FOR 
      ttSysCtrlUsage SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE sysCtrlUsage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS sysCtrlUsage C-Win _FREEFORM
  QUERY sysCtrlUsage DISPLAY
      ttSysCtrlUsage
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 160 BY 27.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnClearSysCtrlUsage AT ROW 1 COL 1 WIDGET-ID 2
     btnStackTrace AT ROW 1 COL 26 WIDGET-ID 4
     sysCtrlUsage AT ROW 1.95 COL 1 WIDGET-ID 200
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57 WIDGET-ID 100.


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
         TITLE              = "User SysCtrl Usage"
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB sysCtrlUsage btnStackTrace DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE sysCtrlUsage
/* Query rebuild information for BROWSE sysCtrlUsage
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSysCtrlUsage.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE sysCtrlUsage */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* User SysCtrl Usage */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* User SysCtrl Usage */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* User SysCtrl Usage */
DO:
    RUN pReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearSysCtrlUsage
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearSysCtrlUsage C-Win
ON CHOOSE OF btnClearSysCtrlUsage IN FRAME DEFAULT-FRAME /* Clear SysCtrl Usage */
DO:
    DYNAMIC-FUNCTION("sfClearTtSysCtrlUsage").
    RUN pGetSysCtrlUsage.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStackTrace
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStackTrace C-Win
ON CHOOSE OF btnStackTrace IN FRAME DEFAULT-FRAME /* View Stack Trace */
DO:
    IF AVAILABLE ttSysCtrlUsage THEN
    RUN system/stackTrace.w (ttSysCtrlUsage.stackTrace).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME sysCtrlUsage
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
  DYNAMIC-FUNCTION("sfSetSysCtrlUsageHandle", THIS-PROCEDURE).
  RUN pGetSysCtrlUsage.
  RUN enable_UI.
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
  ENABLE btnClearSysCtrlUsage btnStackTrace sysCtrlUsage 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSysCtrlUsage C-Win 
PROCEDURE pGetSysCtrlUsage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hSysCtrlUsage AS HANDLE  NO-UNDO EXTENT 2.
    DEFINE VARIABLE hQuery        AS HANDLE  NO-UNDO EXTENT 2.
    DEFINE VARIABLE idx           AS INTEGER NO-UNDO.
    
    EMPTY TEMP-TABLE ttSysCtrlUsage.
    
    ASSIGN
        hSysCtrlUsage[1] = DYNAMIC-FUNCTION("sfGetTtSysCtrlUsageHandle")
        hSysCtrlUsage[1] = hSysCtrlUsage[1]:DEFAULT-BUFFER-HANDLE
        hSysCtrlUsage[2] = TEMP-TABLE ttSysCtrlUsage:HANDLE
        hSysCtrlUsage[2] = hSysCtrlUsage[2]:DEFAULT-BUFFER-HANDLE
        .    
    /* scroll returned temp-table records */
    CREATE QUERY hQuery[1].
    hQuery[1]:SET-BUFFERS(hSysCtrlUsage[1]:HANDLE).
    hQuery[1]:QUERY-PREPARE("FOR EACH " + hSysCtrlUsage[1]:NAME).
    hQuery[1]:QUERY-OPEN.

    CREATE QUERY hQuery[2].
    hQuery[2]:SET-BUFFERS(hSysCtrlUsage[2]:HANDLE).

    REPEAT:
        hQuery[1]:GET-NEXT().
        IF hQuery[1]:QUERY-OFF-END THEN LEAVE.
        CREATE ttSysCtrlUsage.
        DO idx = 1 TO hSysCtrlUsage[1]:NUM-FIELDS:
            hSysCtrlUsage[2]:BUFFER-FIELD(idx):BUFFER-VALUE() = hSysCtrlUsage[1]:BUFFER-FIELD(idx):BUFFER-VALUE(). 
        END. /* do idx */
    END. /* repeat */
    hQuery[1]:QUERY-CLOSE().
    DELETE OBJECT hQuery[1].
    
    {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReSize C-Win 
PROCEDURE pReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FRAME {&FRAME-NAME}:HIDDEN = YES.
    ASSIGN
        FRAME {&FRAME-NAME}:VIRTUAL-WIDTH = {&WINDOW-NAME}:WIDTH
        FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
        FRAME {&FRAME-NAME}:WIDTH = {&WINDOW-NAME}:WIDTH
        FRAME {&FRAME-NAME}:HEIGHT = {&WINDOW-NAME}:HEIGHT
        BROWSE {&BROWSE-NAME}:WIDTH = FRAME {&FRAME-NAME}:WIDTH
        BROWSE {&BROWSE-NAME}:HEIGHT = FRAME {&FRAME-NAME}:HEIGHT - FRAME {&FRAME-NAME}:ROW
        .
    FRAME {&FRAME-NAME}:HIDDEN = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

