&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME ProgressBarWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS ProgressBarWindow 
/*------------------------------------------------------------------------

  File: system/progressBar.w

  Description: Progress Status Bar

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 2.10.2021

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME ProgressBarFrame

/* Standard List Definitions                                            */
&Scoped-Define DISPLAYED-OBJECTS progressBarPct 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR ProgressBarWindow AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE progressBarPct AS CHARACTER FORMAT "X(4)":U INITIAL "0%" 
      VIEW-AS TEXT 
     SIZE 5 BY .62
     BGCOLOR 10  NO-UNDO.

DEFINE RECTANGLE progressBar
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 6 BY 1
     BGCOLOR 10 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME ProgressBarFrame
     progressBarPct AT ROW 1.24 COL 1 NO-LABEL WIDGET-ID 8
     progressBar AT ROW 1 COL 1 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         NO-LABELS SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 100 BY 1 WIDGET-ID 100.


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
  CREATE WINDOW ProgressBarWindow ASSIGN
         HIDDEN             = YES
         TITLE              = "Progress Bar"
         HEIGHT             = 1
         WIDTH              = 100
         MAX-HEIGHT         = 1
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 1
         VIRTUAL-WIDTH      = 100
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
/* SETTINGS FOR WINDOW ProgressBarWindow
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME ProgressBarFrame
   FRAME-NAME                                                           */
/* SETTINGS FOR RECTANGLE progressBar IN FRAME ProgressBarFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN progressBarPct IN FRAME ProgressBarFrame
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(ProgressBarWindow)
THEN ProgressBarWindow:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME ProgressBarFrame
/* Query rebuild information for FRAME ProgressBarFrame
     _Query            is NOT OPENED
*/  /* FRAME ProgressBarFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME ProgressBarWindow
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ProgressBarWindow ProgressBarWindow
ON END-ERROR OF ProgressBarWindow /* Progress Bar */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ProgressBarWindow ProgressBarWindow
ON WINDOW-CLOSE OF ProgressBarWindow /* Progress Bar */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK ProgressBarWindow 


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
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI ProgressBarWindow  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(ProgressBarWindow)
  THEN DELETE WIDGET ProgressBarWindow.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI ProgressBarWindow  _DEFAULT-ENABLE
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
  DISPLAY progressBarPct 
      WITH FRAME ProgressBarFrame IN WINDOW ProgressBarWindow.
  VIEW FRAME ProgressBarFrame IN WINDOW ProgressBarWindow.
  {&OPEN-BROWSERS-IN-QUERY-ProgressBarFrame}
  VIEW ProgressBarWindow.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pProgressBar ProgressBarWindow 
PROCEDURE pProgressBar :
/*------------------------------------------------------------------------------
 Purpose: generic progress bar
 Notes: if count and total are unknown, count should be ?, total is treated as percent
        if total is unknown, set total to ? and it displays Phase X in title
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTitle   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiCount AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiTotal   AS INTEGER   NO-UNDO.

    DEFINE VARIABLE cTitle   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dPercent AS DECIMAL   NO-UNDO.

    ASSIGN
        cTitle = IF ipiCount EQ ? THEN "" 
                 ELSE IF ipiTotal EQ ? THEN " - Phase " + ENTRY(1,STRING(ipiCount / 2000 + 1),".")
                 ELSE " - " + STRING(ipiCount) + " of " + STRING(ipiTotal)
        {&WINDOW-NAME}:TITLE = ipcTitle + cTitle
        ipiCount = IF ipiTotal EQ ? THEN ipiCount MOD 2000 ELSE ipiCount
        ipiTotal = IF ipiTotal EQ ? THEN 2000 ELSE ipiTotal
        dPercent = IF ipiCount EQ ? THEN ipiTotal
                   ELSE (ipiCount / ipiTotal) * 100
        dPercent = ROUND(dPercent,0)
        progressBar:WIDTH IN FRAME {&FRAME-NAME} = IF dPercent LT 6 THEN 6
                                                   ELSE dPercent
        progressBarPct:SCREEN-VALUE = STRING(dPercent) + "%"
        progressBarPct:COL = progressBar:WIDTH - progressBarPCt:WIDTH
        .
    /* need this to prevent windows NOT RESPONDING from taking control */
    IF dPercent MOD 25 EQ 0 THEN
    PROCESS EVENTS.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

