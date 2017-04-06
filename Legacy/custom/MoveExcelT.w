&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File              : custom/MoveExcelT.w

  Description       : Move Excel Template

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
{methods/defines/hndldefs.i}
{custom/gcompany.i}
{custom/getcmpny.i}
{sys/inc/var.i new shared}

assign
 cocode = gcompany.

/* Variables */
DEFINE VARIABLE vcStartingPath  AS CHARACTER  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-42 fi_filename btnOk BtnCancel 
&Scoped-Define DISPLAYED-OBJECTS fi_filename 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnOk AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fi_filename AS CHARACTER FORMAT "x(100)":U 
     LABEL "Filename" 
     VIEW-AS FILL-IN 
     SIZE 51.4 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-42
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 70 BY 3.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fi_filename AT ROW 3.14 COL 14.6 COLON-ALIGNED
     btnOk AT ROW 12.19 COL 25
     BtnCancel AT ROW 12.19 COL 41.2
     "Move Excel Template" VIEW-AS TEXT
          SIZE 21.4 BY .62 AT ROW 2.1 COL 6.6
     RECT-42 AT ROW 2.38 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 74.4 BY 12.62.


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
         TITLE              = "Move Excel Template"
         HEIGHT             = 12.62
         WIDTH              = 74.4
         MAX-HEIGHT         = 12.62
         MAX-WIDTH          = 74.4
         VIRTUAL-HEIGHT     = 12.62
         VIRTUAL-WIDTH      = 74.4
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
                                                                        */
ASSIGN
       BtnCancel:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


ASSIGN
       btnOk:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Move Excel Template */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Move Excel Template */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel C-Win
ON CHOOSE OF BtnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
    APPLY "close" TO THIS-PROCEDURE.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 03.28.2017 @ 10:42:46 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOk C-Win
ON CHOOSE OF btnOk IN FRAME DEFAULT-FRAME /* OK */
DO:
   DEF VAR v-process AS LOG NO-UNDO.

   ASSIGN fi_filename 
          FILE-INFO:FILE-NAME = fi_filename.

   IF FILE-INFO:FILE-TYPE EQ ? THEN
   DO:
       MESSAGE "Filename is invalid."
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY "ENTRY":U TO fi_filename IN FRAME {&FRAME-NAME}.
       LEAVE.
   END.

   message "Are you sure you want copy Excel Template file?"     
          view-as alert-box question button yes-no update v-process.

  if v-process then run run-process.

    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p on 03.28.2017 @ 10:42:46 am */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_filename
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_filename C-Win
ON HELP OF fi_filename IN FRAME DEFAULT-FRAME /* Filename */
DO:
  DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
  DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
  DEF VAR v-file-name AS CHAR NO-UNDO.

  ASSIGN
    init-dir = "c:\asi_gui10\pco1010\template" .
  SYSTEM-DIALOG GET-FILE v-file-name
      TITLE      "Select Excel Template  ..."
      FILTERS    "Excel Files (*.xlt)" "*.xlt",
                 "All Files (*.*)" "*.*"
      INITIAL-DIR init-dir

      USE-FILENAME
      UPDATE OKpressed.
  IF NOT OKpressed THEN
     RETURN NO-APPLY.

  fi_filename:SCREEN-VALUE = v-file-name.
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
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p on 03.28.2017 @ 10:42:46 am */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   RUN enable_UI.

   apply 'entry':u to fi_filename.

    {methods/setButton.i BtnCancel "Cancel"} /* added by script _nonAdm1Images.p on 03.28.2017 @ 10:43:31 am */
    {methods/setButton.i btnOk "OK"} /* added by script _nonAdm1Images.p on 03.28.2017 @ 10:43:31 am */
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p on 03.28.2017 @ 10:42:46 am */
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
  DISPLAY fi_filename 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-42 fi_filename btnOk BtnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR vcCommand AS CHAR NO-UNDO.
  DEF VAR i AS INT INIT 1 NO-UNDO.
  DEF VAR newi AS INT INIT 0 NO-UNDO.
  DEF VAR v-filename AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

     SESSION:SET-WAIT-STATE ("general").

     REPEAT:
        i = INDEX(fi_filename,"\",newi + 1).
        IF i = 0 THEN
           LEAVE.
        newi = i.
     END.

     v-filename = SUBSTRING(fi_filename,newi + 1).

     vcCommand = "attrib -R " + "P:\asi10test\pco1010\template\" + v-filename.
     OS-COMMAND SILENT VALUE (vcCommand).

     vcCommand = "copy " + fi_filename + " P:\asi10test\pco1010\template\".
     OS-COMMAND SILENT VALUE (vcCommand).

     vcCommand = "attrib -R " + "P:\asi10test\rco1010\template\" + v-filename.
     OS-COMMAND SILENT VALUE (vcCommand).

     vcCommand = "copy " + fi_filename + " P:\asi10test\rco1010\template\".
     OS-COMMAND SILENT VALUE (vcCommand).

     vcCommand = "attrib -R " + "P:\asi10test\patch\pco1010\template\" + v-filename.
     OS-COMMAND SILENT VALUE (vcCommand).

     vcCommand = "copy " + fi_filename + " P:\asi10test\patch\pco1010\template\".
     OS-COMMAND SILENT VALUE (vcCommand).

     vcCommand = "attrib -R " + "P:\asi10test\patch\rco1010\template\" + v-filename.
     OS-COMMAND SILENT VALUE (vcCommand).

     vcCommand = "copy " + fi_filename + " P:\asi10test\patch\rco1010\template\".
     OS-COMMAND SILENT VALUE (vcCommand).

     vcCommand = "attrib -R " + "P:\asi10ship\rco1010\template\" + v-filename.
     OS-COMMAND SILENT VALUE (vcCommand).

     vcCommand = "copy " + fi_filename + " P:\asi10ship\rco1010\template\".
     OS-COMMAND SILENT VALUE (vcCommand).

     vcCommand = "attrib -R " + "P:\asi10ship\patch\rco1010\template\" + v-filename.
     OS-COMMAND SILENT VALUE (vcCommand).

     vcCommand = "copy " + fi_filename + " P:\asi10ship\patch\rco1010\template\".
     OS-COMMAND SILENT VALUE (vcCommand).

     MESSAGE "Files were copied."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

