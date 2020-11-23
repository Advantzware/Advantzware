&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: screenViewer.w

  Description: Screen Viewer

  Input Parameters: List Name, Title, Font and Orientation

  Output Parameters: <none>

  Author: Ron Stark

  Created: 9.25.2020 (re-created from scr-rpt.w dialog version)

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

&Scoped-define program-id scr-rpt.

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ipcListName    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcTitle       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiFont        AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER ipcOrientation AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE iFont        AS INTEGER   NO-UNDO INITIAL 10.
DEFINE VARIABLE cOrientation AS CHARACTER NO-UNDO INITIAL "P".
DEFINE VARIABLE gcompany     AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{custom/getcmpny.i}

ASSIGN
    iFont        = ipiFont
    cOrientation = ipcOrientation
    .
DO TRANSACTION:
   {sys/inc/notepad.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn-close ed-scr-view btn-font btn-print ~
btn-save 
&Scoped-Define DISPLAYED-OBJECTS ed-scr-view 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-close AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Exit".

DEFINE BUTTON btn-font 
     IMAGE-UP FILE "Graphics/32x32/font.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Font".

DEFINE BUTTON btn-print 
     IMAGE-UP FILE "Graphics/32x32/printer.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Print".

DEFINE BUTTON btn-save 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Save As".

DEFINE VARIABLE ed-scr-view AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 160 BY 26.19
     FONT 9 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 34 BY 2.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btn-close AT ROW 27.43 COL 152 WIDGET-ID 4
     ed-scr-view AT ROW 1 COL 1 NO-LABEL WIDGET-ID 2
     btn-font AT ROW 27.43 COL 144 WIDGET-ID 6
     btn-print AT ROW 27.43 COL 128 WIDGET-ID 8
     btn-save AT ROW 27.43 COL 136 WIDGET-ID 10
     RECT-1 AT ROW 27.19 COL 127 WIDGET-ID 12
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
         TITLE              = "Viewer"
         HEIGHT             = 28.57
         WIDTH              = 160
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = YES
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Viewer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Viewer */
DO:
  /* This event will close the window and terminate the procedure.  */
  RUN pSaveSettings.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Viewer */
DO:
    RUN pWinReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-close C-Win
ON CHOOSE OF btn-close IN FRAME DEFAULT-FRAME
DO:
    RUN pSaveSettings.
    APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-font
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-font C-Win
ON CHOOSE OF btn-font IN FRAME DEFAULT-FRAME
DO:
  FONT-TABLE:NUM-ENTRIES = 99.
  SYSTEM-DIALOG FONT 9 FIXED-ONLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-print C-Win
ON CHOOSE OF btn-print IN FRAME DEFAULT-FRAME
DO:
    RUN custom/prntproc.p (ipcListName, iFont, cOrientation).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-save C-Win
ON CHOOSE OF btn-save IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE OKpressed AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.

    init-dir = "c:\temp\" .
    SYSTEM-DIALOG GET-FILE ipcListName
        TITLE   "Enter Listing Name to SAVE AS ..."
        FILTERS "Listing Files (*.rpt)" "*.rpt",
                "All Files (*.*)" "*.*"
        INITIAL-DIR init-dir
        ASK-OVERWRITE
        CREATE-TEST-FILE
        SAVE-AS
        USE-FILENAME
        UPDATE OKpressed.
    IF NOT OKpressed THEN
    RETURN NO-APPLY.
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
  {&WINDOW-NAME}:TITLE = ipcTitle.
  IF notepad-log THEN DO:
      IF notepad-chr EQ "" THEN DO: /* task 02101509 */
&IF DEFINED(FWD-VERSION) > 0 &THEN
          open-mime-resource "text/plain" STRING("file:///" + ipcListName) FALSE.
&ELSE
          OS-COMMAND NO-WAIT notepad VALUE(ipcListName).
&ENDIF
          APPLY "CLOSE":U TO THIS-PROCEDURE.
          RETURN.
      END.
      ELSE DO:
          FIND FIRST usergrps NO-LOCK
               WHERE usergrps.usergrps EQ "Notepad" 
               NO-ERROR.
          IF AVAILABLE usergrps AND LOOKUP(STRING(USERID(LDBNAME(1))),usergrps.users) <> 0 THEN DO:
&IF DEFINED(FWD-VERSION) > 0 &THEN
              open-mime-resource "text/plain" STRING("file:///" + ipcListName) FALSE.
&ELSE
              OS-COMMAND NO-WAIT notepad VALUE(ipcListName).
&ENDIF
              APPLY "CLOSE":U TO THIS-PROCEDURE.
              RETURN.
          END.
          ELSE DO:
              IF NOT AVAILABLE usergrps THEN DO:
                  CREATE usergrps.
                  ASSIGN
                      usergrps.usergrps = "Notepad"
                      usergrps.dscr     = "NOTEPAD USERS"
                      .
              END. /* if not avail */
          END.  /* not avail usergrps */
      END. /* else do  notepad-chr = "" */
  END.  /* if notepad-log */
  RUN pGetSettings.
  RUN enable_UI.
  ed-scr-view:READ-FILE(ipcListName).
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
  DISPLAY ed-scr-view 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btn-close ed-scr-view btn-font btn-print btn-save 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSettings C-Win 
PROCEDURE pGetSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    DEFINE BUFFER user-print FOR user-print.

    FIND FIRST user-print NO-LOCK
         WHERE user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ USERID("ASI")
         NO-ERROR.
    IF AVAILABLE user-print THEN DO:
        DO idx = 1 TO EXTENT(user-print.field-name):
            IF user-print.field-name[idx] EQ "" THEN LEAVE.
            CASE user-print.field-name[idx]:
                WHEN "WindowHeight" THEN
                ASSIGN
                    {&WINDOW-NAME}:HEIGHT = DECIMAL(user-print.field-value[idx])
                    FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
                    .
                WHEN "WindowWidth" THEN
                ASSIGN
                    {&WINDOW-NAME}:WIDTH = DECIMAL(user-print.field-value[idx])
                    FRAME {&FRAME-NAME}:VIRTUAL-WIDTH = {&WINDOW-NAME}:WIDTH
                    .
            END CASE.
        END. /* do idx */
    END. /* if avail */
    RELEASE user-print.
    RUN pWinReSize.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveSettings C-Win 
PROCEDURE pSaveSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    DEFINE BUFFER user-print FOR user-print.

    DO TRANSACTION:
        FIND FIRST user-print EXCLUSIVE-LOCK
             WHERE user-print.program-id EQ "{&program-id}"
               AND user-print.user-id    EQ USERID("ASI")
             NO-ERROR.
        IF NOT AVAILABLE user-print THEN DO:
            CREATE user-print.
            ASSIGN
                user-print.program-id = "{&program-id}"
                user-print.user-id    = USERID("ASI")
                .
        END. /* not avail */
        ASSIGN
            user-print.field-name  = ""
            user-print.field-value = ""
            user-print.field-label = ""
            .
        ASSIGN
            idx = idx + 1
            user-print.field-name[idx]  = "WindowWidth"
            user-print.field-label[idx] = "WindowWidth"
            user-print.field-value[idx] = STRING(FRAME {&FRAME-NAME}:WIDTH)
            idx = idx + 1
            user-print.field-name[idx]  = "WindowHeight"
            user-print.field-label[idx] = "WindowHeight"
            user-print.field-value[idx] = STRING(FRAME {&FRAME-NAME}:HEIGHT)
            .
        RELEASE user-print.
    END. /* do trans */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinReSize C-Win 
PROCEDURE pWinReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*    SESSION:SET-WAIT-STATE("General").                                */
/*    DO WITH FRAME {&FRAME-NAME}:                                      */
/*        HIDE FRAME {&FRAME-NAME}.                                     */
/*        IF {&WINDOW-NAME}:HEIGHT LT 28.57 THEN                        */
/*        {&WINDOW-NAME}:HEIGHT = 28.57.                                */
/*        IF {&WINDOW-NAME}:WIDTH  LT 160   THEN                        */
/*        {&WINDOW-NAME}:WIDTH  = 160.                                  */
/*        ASSIGN                                                        */
/*            {&WINDOW-NAME}:COL = 1                                    */
/*            {&WINDOW-NAME}:ROW = 1                                    */
/*            RECT-1:HIDDEN      = YES                                  */
/*            btn-print:HIDDEN   = YES                                  */
/*            btn-save:HIDDEN    = YES                                  */
/*            btn-font:HIDDEN    = YES                                  */
/*            btn-close:HIDDEN   = YES                                  */
/*            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT*/
/*            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH */
/*            FRAME {&FRAME-NAME}:HEIGHT = {&WINDOW-NAME}:HEIGHT        */
/*            FRAME {&FRAME-NAME}:WIDTH  = {&WINDOW-NAME}:WIDTH         */
/*            ed-scr-view:HEIGHT = FRAME {&FRAME-NAME}:HEIGHT - 2.38    */
/*            ed-scr-view:WIDTH  = FRAME {&FRAME-NAME}:WIDTH            */
/*            RECT-1:ROW         = FRAME {&FRAME-NAME}:HEIGHT - 1.4     */
/*            btn-print:ROW      = RECT-1:ROW    + .24                  */
/*            btn-save:ROW       = btn-print:ROW                        */
/*            btn-font:ROW       = btn-save:ROW                         */
/*            btn-close:ROW      = btn-font:ROW                         */
/*            RECT-1:COL         = FRAME {&FRAME-NAME}:WIDTH  - 33      */
/*            btn-print:COL      = RECT-1:COL    + 1                    */
/*            btn-save:COL       = btn-print:COL + btn-print:WIDTH      */
/*            btn-font:COL       = btn-save:COL  + btn-save:WIDTH       */
/*            btn-close:COL      = btn-font:COL  + btn-font:WIDTH       */
/*            .                                                         */
/*        VIEW FRAME {&FRAME-NAME}.                                     */
/*        ASSIGN                                                        */
/*            RECT-1:HIDDEN    = NO                                     */
/*            btn-print:HIDDEN = NO                                     */
/*            btn-save:HIDDEN  = NO                                     */
/*            btn-font:HIDDEN  = NO                                     */
/*            btn-close:HIDDEN = NO                                     */
/*            .                                                         */
/*    END. /* do with */                                                */
/*    SESSION:SET-WAIT-STATE("").                                       */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

