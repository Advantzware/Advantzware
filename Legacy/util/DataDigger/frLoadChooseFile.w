&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

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

{ datadigger.i }

/* Parameters Definitions ---                                           */
define input  parameter phParent as handle      no-undo.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frChooseFile

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiDataFile btnChooseDir 
&Scoped-Define DISPLAYED-OBJECTS fiDataFile fiWarning 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnChooseDir 
     LABEL "..." 
     SIZE-PIXELS 20 BY 21 TOOLTIP "choose directory".

DEFINE VARIABLE fiDataFile AS CHARACTER 
     LABEL "Data &file" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     SIMPLE
     SIZE 93.6 BY 1 TOOLTIP "the file to load"
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE fiWarning AS CHARACTER FORMAT "X(256)":U INITIAL "Not a valid file name" 
      VIEW-AS TEXT 
     SIZE 23 BY .95
     FGCOLOR 12  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frChooseFile
     fiDataFile AT ROW 6.24 COL 16.4 WIDGET-ID 18
     btnChooseDir AT Y 110 X 610 WIDGET-ID 4
     fiWarning AT ROW 7.43 COL 29 COLON-ALIGNED NO-LABEL WIDGET-ID 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT X 0 Y 0
         SIZE-PIXELS 649 BY 275 DROP-TARGET WIDGET-ID 100.


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
         TITLE              = "Import"
         HEIGHT-P           = 275
         WIDTH-P            = 653
         MAX-HEIGHT-P       = 1181
         MAX-WIDTH-P        = 1920
         VIRTUAL-HEIGHT-P   = 1181
         VIRTUAL-WIDTH-P    = 1920
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



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME frChooseFile
   FRAME-NAME                                                           */
/* SETTINGS FOR COMBO-BOX fiDataFile IN FRAME frChooseFile
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fiWarning IN FRAME frChooseFile
   NO-ENABLE                                                            */
ASSIGN 
       fiWarning:READ-ONLY IN FRAME frChooseFile        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Import */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Import */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME frChooseFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frChooseFile C-Win
ON DROP-FILE-NOTIFY OF FRAME frChooseFile
DO:
  /* Turn off warning */
  fiWarning:visible = false.

  fiDataFile:screen-value = frame frChooseFile:get-dropped-file(1).
  frame frChooseFile:end-file-drop(). 
  apply 'value-changed' to fiDataFile in frame frChooseFile.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL frChooseFile C-Win
ON RETURN OF FRAME frChooseFile
DO:
  apply 'go' to current-window.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnChooseDir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnChooseDir C-Win
ON CHOOSE OF btnChooseDir IN FRAME frChooseFile /* ... */
DO:
  define variable cDir  as character   no-undo.
  define variable cFile as character   no-undo.
  define variable lOk   as logical     no-undo.

  /* Turn off warning */
  fiWarning:visible = false.

  cDir = fiDataFile:screen-value.
  if cDir = ? then cDir = ''.

  if num-entries(cDir,'\') > 0 then
    entry(num-entries(cDir,'\'),cDir,'\') = ''.

  system-dialog get-file cFile
        title       "Choose file to load"
        filters     "All data files (*.d,*.xml,*.csv)" "*.d,*.xml,*.csv",
                    "All files      (*.*)"    "*.*",
                    "CSV files      (*.csv)"  "*.csv",
                    "Progress files (*.d)"    "*.d",
                    "XML files      (*.xml)"  "*.xml"
        initial-filter 1
        must-exist
        use-filename
        initial-dir cDir
        update lOk
    .
  if lOk then
    fiDataFile:screen-value = cFile.

    {Advantzware/WinKit/winkit-panel-triggerend.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiDataFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiDataFile C-Win
ON ENTRY OF fiDataFile IN FRAME frChooseFile /* Data file */
DO:
  fiWarning:visible = false.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiDataFile C-Win
ON VALUE-CHANGED OF fiDataFile IN FRAME frChooseFile /* Data file */
DO:
  self:fgcolor = (if search(self:screen-value) <> ? then ? else 12).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

DELETE WIDGET  {&WINDOW-NAME}.
{&WINDOW-NAME} = CURRENT-WINDOW.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i}
END.

/* General procedures for all viewers in the MVC */
{ frLoad.i }

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  /* Get file from registry. Our parent might have 
   * put it there 
   */
  fiDataFile = getRegistry('DumpAndLoad', 'LoadFile').

/*  fiDataFile = 'd:\Data\Progress\DataDigger\lbl_mstr.d'. /* debug */ */

  RUN enable_UI.
  fiWarning:visible = false.

    {Advantzware/WinKit/embedfinalize-nonadm.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE commitPage C-Win 
PROCEDURE commitPage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define output parameter plTabOk as logical     no-undo.

  do with frame frChooseFile:
    plTabOk = search(fiDataFile:screen-value) <> ?.

    if not plTabOk then 
      fiWarning:visible = true.
    else 
      setData('DataFile',fiDataFile:screen-value).

  end.

END PROCEDURE. /* commitPage */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY fiDataFile fiWarning 
      WITH FRAME frChooseFile IN WINDOW C-Win.
  ENABLE fiDataFile btnChooseDir 
      WITH FRAME frChooseFile IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frChooseFile}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
