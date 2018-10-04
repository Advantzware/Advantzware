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
&SCOPED-DEFINE DontValidateError /* added by script _dontValidatePanels.p */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 dataFile btnBrowse btnOK btnClose 
&Scoped-Define DISPLAYED-OBJECTS dataFile 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnBrowse 
     LABEL "&Browse" 
     SIZE 15 BY 1.

DEFINE BUTTON btnClose 
     LABEL "&Close" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnOK 
     LABEL "&OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE dataFile AS CHARACTER FORMAT "X(256)":U 
     LABEL "Data File" 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 1.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     dataFile AT ROW 1.24 COL 10 COLON-ALIGNED
     btnBrowse AT ROW 1.24 COL 64
     btnOK AT ROW 3.38 COL 48
     btnClose AT ROW 3.38 COL 64
     RECT-6 AT ROW 3.14 COL 47
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 4.1.


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
         TITLE              = "Mid York Press Data Load"
         HEIGHT             = 4.1
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN
       btnClose:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


ASSIGN
       btnOK:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Mid York Press Data Load */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Mid York Press Data Load */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBrowse C-Win
ON CHOOSE OF btnBrowse IN FRAME DEFAULT-FRAME /* Browse */
DO:
  RUN selectDataFile.
  APPLY 'LEAVE':U TO dataFile.
  RETURN NO-APPLY.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClose C-Win
ON CHOOSE OF btnClose IN FRAME DEFAULT-FRAME /* Close */
DO:
  APPLY 'CLOSE':U TO THIS-PROCEDURE.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK C-Win
ON CHOOSE OF btnOK IN FRAME DEFAULT-FRAME /* OK */
DO:
  RUN processFile.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dataFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dataFile C-Win
ON LEAVE OF dataFile IN FRAME DEFAULT-FRAME /* Data File */
DO:
  ASSIGN {&SELF-NAME}.
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
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
    {methods/setButton.i btnClose "Close"} /* added by script _nonAdm1Images1.p */
    {methods/setButton.i btnOK "OK"} /* added by script _nonAdm1Images1.p */
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
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
  DISPLAY dataFile 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-6 dataFile btnBrowse btnOK btnClose 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processFile C-Win 
PROCEDURE processFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE headerLine AS CHARACTER NO-UNDO.
  DEFINE VARIABLE fieldName AS CHARACTER NO-UNDO EXTENT 200.
  DEFINE VARIABLE dataField AS CHARACTER NO-UNDO EXTENT 200.
  DEFINE VARIABLE tableName AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dataProgram AS CHARACTER NO-UNDO.
  DEFINE VARIABLE idx AS INTEGER NO-UNDO.
  DEFINE VARIABLE cnt AS INTEGER NO-UNDO.

  INPUT FROM VALUE(dataFile) NO-ECHO.
  IMPORT UNFORMATTED headerLine.
  INPUT CLOSE.

  ASSIGN
    dataProgram = REPLACE(dataFile,'.txt','.p')
    tableName = SUBSTR(headerLine,1,INDEX(headerLine,'.') - 1)
    cnt = NUM-ENTRIES(headerLine,'~t').
  DO idx = 1 TO cnt:
    fieldName[idx] = ENTRY(idx,headerLine,'~t').
  END. /* do idx */
  FIND FIRST asi._file NO-LOCK WHERE asi._file._file-name EQ tableName NO-ERROR.
  IF NOT AVAILABLE asi._file THEN DO:
    MESSAGE 'Unable to determine Table Name' VIEW-AS ALERT-BOX ERROR.
    RETURN.
  END.

  OUTPUT TO VALUE(dataProgram).
  PUT UNFORMATTED
    '/* ' dataProgram ' */' SKIP(1)
    'DEFINE VARIABLE dataField AS CHARACTER NO-UNDO EXTENT ' cnt '.' SKIP(1)
    'MESSAGE ~'Clear Table "' tableName '" Before Load?~'' SKIP
    '  VIEW-AS ALERT-BOX BUTTONS YES-NO-CANCEL' SKIP
    '  UPDATE clearTable AS LOGICAL.' SKIP
    'CASE clearTable:' SKIP
    '  WHEN YES THEN' SKIP
    '  FOR EACH ' tableName ' EXCLUSIVE-LOCK:' SKIP
    '    DELETE ' tableName '.' SKIP
    '  END.' SKIP
    '  WHEN ? THEN RETURN.' SKIP
    'END CASE.' SKIP(1)
    'INPUT FROM "' dataFile '" NO-ECHO.' SKIP
    'IMPORT ^. /* skip header line */' SKIP
    'REPEAT:' SKIP
    '  IMPORT DELIMITER "~~t"' SKIP.
  DO idx = 1 TO cnt:
    PUT UNFORMATTED '    dataField[' idx ']' SKIP.
  END. /* do idx */
  PUT UNFORMATTED '    .' SKIP
    '  CREATE ' tableName '.' SKIP
    '  CREATE reftable.' SKIP
    "  ASSIGN reftable.reftable = 'PREPCADFILE' " SKIP
    "  reftable.rec_key = prep.rec_key" SKIP.

  DO idx = 1 TO cnt:

    IF SUBSTR(fieldName[idx],INDEX(fieldName[idx],'.') + 1) EQ "Cad#" THEN
       PUT UNFORMATTED '     reftable.CODE = ' 'dataField[' idx ']' SKIP.
    ELSE IF SUBSTR(fieldName[idx],INDEX(fieldName[idx],'.') + 1) EQ "File#" THEN
       PUT UNFORMATTED '     reftable.CODE2 = ' 'dataField[' idx ']' SKIP.
    ELSE IF SUBSTR(fieldName[idx],INDEX(fieldName[idx],'.') + 1) EQ "Owner" THEN
       PUT UNFORMATTED '     prep.owner[1] = ' 'dataField[' idx ']' SKIP.
    ELSE IF SUBSTR(fieldName[idx],INDEX(fieldName[idx],'.') + 1) EQ "owner-%" THEN
       PUT UNFORMATTED '     prep.owner-%[1] = INTEGER('   'dataField[' idx '])'  SKIP.
    ELSE
    DO:
       FIND FIRST asi._field OF asi._file NO-LOCK
            WHERE asi._field._field-name EQ SUBSTR(fieldName[idx],INDEX(fieldName[idx],'.') + 1) NO-ERROR.

       IF NOT AVAILABLE asi._field THEN NEXT.
       PUT UNFORMATTED '    ' fieldName[idx] ' = '.
       IF asi._field._data-type EQ 'logical' THEN
       PUT UNFORMATTED 'dataField[' idx '] EQ "1"'.
       ELSE
       IF asi._field._data-type NE 'character' THEN
       PUT UNFORMATTED CAPS(asi._field._data-type) '('.
       IF asi._field._data-type NE 'logical' THEN
       PUT UNFORMATTED 'dataField[' idx ']'.
       IF asi._field._data-type NE 'character' AND
          asi._field._data-type NE 'logical' THEN
       PUT UNFORMATTED ')'.
       PUT UNFORMATTED SKIP.
    END.
  END. /* do idx */
  PUT UNFORMATTED '    .' SKIP
    'END.' SKIP
    'INPUT CLOSE.' SKIP
    'MESSAGE "Program: ' dataProgram ' - Run Complete" VIEW-AS ALERT-BOX.' SKIP.
  OUTPUT CLOSE.

  MESSAGE 'Run Program:' dataProgram + '?'
    VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE runProgram AS LOGICAL.
  IF runProgram THEN
  DO:
     SESSION:SET-WAIT-STATE("general").
     RUN VALUE(dataProgram).
     SESSION:SET-WAIT-STATE("").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selectDataFile C-Win 
PROCEDURE selectDataFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dataFileName AS CHARACTER NO-UNDO.
  DEFINE VARIABLE initDir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE okPressed AS LOGICAL NO-UNDO.

  initDir = './'.
  SYSTEM-DIALOG GET-FILE dataFileName
      TITLE      'Choose Text (Data File) to SELECT ...'
      FILTERS    'Text Files (*.txt)' '*.txt',
                 'All Files (*.*)' '*.*'
      INITIAL-DIR initDir
      MUST-EXIST
      USE-FILENAME
      UPDATE okPressed.
  IF NOT okPressed THEN
  RETURN NO-APPLY.

  dataFile:SCREEN-VALUE IN FRAME {&FRAME-NAME} = dataFileName.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

