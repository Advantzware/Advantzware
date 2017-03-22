&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: scr_view.w

  Description: Screen Viewer

  Input Parameters: File Name to View

  Output Parameters: <none>

  Author: Ron Stark

  Created: 02/14/98

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) = 0 &THEN
DEFINE INPUT PARAMETER screen-file-name AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE screen-file-name AS CHARACTER NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE list-name AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS screen-report RECT-4 RECT-1 RECT-5 RECT-2 ~
Btn_Open Btn_Print Btn_Delete Btn_Save_As Btn_Search Btn_Font Btn_Close 
&Scoped-Define DISPLAYED-OBJECTS screen-report search-string 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Btn_Next Btn_Previous 
&Scoped-define List-2 Btn_Print Btn_Delete Btn_Save_As Btn_Search Btn_Next ~
Btn_Previous 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Close 
     LABEL "&Close" 
     SIZE 10 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Delete 
     LABEL "&Delete" 
     SIZE 10 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Font 
     LABEL "&Font" 
     SIZE 10 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Next 
     LABEL "&Next" 
     SIZE 10 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Open 
     LABEL "&Open" 
     SIZE 10 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Previous 
     LABEL "Pre&v" 
     SIZE 10 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Print 
     LABEL "&Print" 
     SIZE 10 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Save_As 
     LABEL "Save &As" 
     SIZE 10 BY 1.24
     FONT 4.

DEFINE BUTTON Btn_Search 
     LABEL "&Search" 
     SIZE 10 BY 1.24
     FONT 4.

DEFINE VARIABLE screen-report AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 149.4 BY 22.86
     FONT 9 NO-UNDO.

DEFINE VARIABLE search-string AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.24
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 23 BY 1.67.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 77 BY 1.67.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 23 BY 1.67.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 23 BY 1.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     screen-report AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrows Keys to Navigate" NO-LABEL
     Btn_Open AT ROW 24.1 COL 2 HELP
          "Use this function to OPEN another Listing"
     Btn_Print AT ROW 24.1 COL 13 HELP
          "Use this function to PRINT this Listing"
     Btn_Delete AT ROW 24.1 COL 26 HELP
          "Use this function to DELETE this Listing"
     Btn_Save_As AT ROW 24.1 COL 37 HELP
          "Use this function to SAVE this Listing"
     Btn_Search AT ROW 24.1 COL 50 HELP
          "Use this function to enter SEARCH string value"
     search-string AT ROW 24.1 COL 59 COLON-ALIGNED HELP
          "Enter SEARCH value" NO-LABEL
     Btn_Next AT ROW 24.1 COL 104 HELP
          "Use this function to find NEXT occurrence of Search value"
     Btn_Previous AT ROW 24.1 COL 115 HELP
          "Use this function to find PREVIOUS occurrence of Search value"
     Btn_Font AT ROW 24.1 COL 128 HELP
          "Use this function to set FONT Value"
     Btn_Close AT ROW 24.1 COL 139 HELP
          "Use this function to CLOSE this viewer"
     RECT-4 AT ROW 23.86 COL 127
     RECT-1 AT ROW 23.86 COL 1
     RECT-5 AT ROW 23.86 COL 25
     RECT-2 AT ROW 23.86 COL 49
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 149.8 BY 24.76.


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
         TITLE              = "Screen Viewer"
         HEIGHT             = 24.76
         WIDTH              = 149.8
         MAX-HEIGHT         = 24.76
         MAX-WIDTH          = 149.8
         VIRTUAL-HEIGHT     = 24.76
         VIRTUAL-WIDTH      = 149.8
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

IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR BUTTON Btn_Delete IN FRAME DEFAULT-FRAME
   2                                                                    */
/* SETTINGS FOR BUTTON Btn_Next IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR BUTTON Btn_Previous IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR BUTTON Btn_Print IN FRAME DEFAULT-FRAME
   2                                                                    */
/* SETTINGS FOR BUTTON Btn_Save_As IN FRAME DEFAULT-FRAME
   2                                                                    */
/* SETTINGS FOR BUTTON Btn_Search IN FRAME DEFAULT-FRAME
   2                                                                    */
ASSIGN 
       screen-report:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN search-string IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Screen Viewer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Screen Viewer */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Close C-Win
ON CHOOSE OF Btn_Close IN FRAME DEFAULT-FRAME /* Close */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Delete C-Win
ON CHOOSE OF Btn_Delete IN FRAME DEFAULT-FRAME /* Delete */
DO:
  MESSAGE "Delete this Listing?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
      UPDATE deleteok AS LOGICAL.
  IF NOT deleteok THEN
  RETURN NO-APPLY.
  ASSIGN
    screen-report:SCREEN-VALUE = ""
    search-string:SCREEN-VALUE = "".
  OS-DELETE VALUE(list-name).
  DISABLE {&LIST-2} WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Font
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Font C-Win
ON CHOOSE OF Btn_Font IN FRAME DEFAULT-FRAME /* Font */
DO:
  FONT-TABLE:NUM-ENTRIES = 20.
  SYSTEM-DIALOG FONT 9 FIXED-ONLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Next C-Win
ON CHOOSE OF Btn_Next IN FRAME DEFAULT-FRAME /* Next */
DO:
  IF search-string NE "" THEN
  DO:
    ldummy = screen-report:SEARCH(search-string,16).
    IF NOT ldummy THEN
    MESSAGE "Search Match Not Found!" VIEW-AS ALERT-BOX INFORMATION.
    ELSE
    ldummy = screen-report:SET-SELECTION(screen-report:CURSOR-OFFSET -
         LENGTH(search-string:SCREEN-VALUE),screen-report:CURSOR-OFFSET).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Open
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Open C-Win
ON CHOOSE OF Btn_Open IN FRAME DEFAULT-FRAME /* Open */
DO:
  DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
  DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

  ASSIGN
    list-name = screen-file-name + IF screen-file-name NE "" THEN "rpt" ELSE ""
    init-dir = "users\" + USERID("NOSWEAT").
  SYSTEM-DIALOG GET-FILE list-name
      TITLE      "Choose Listing to OPEN ..."
      FILTERS    "Listing Files (*.rpt)" "*.rpt",
                 "All Files (*.*)" "*.*"
      INITIAL-DIR init-dir
      MUST-EXIST
      USE-FILENAME
      UPDATE OKpressed.
  IF NOT OKpressed THEN
  RETURN NO-APPLY.
  ASSIGN
    screen-file-name = SUBSTR(list-name,R-INDEX(list-name,"\") + 1,
        R-INDEX(list-name,".") - R-INDEX(list-name,"\"))
    ldummy = screen-report:READ-FILE(list-name)
    {&WINDOW-NAME}:TITLE = "Screen Viewer - '" + screen-file-name + "'"
    search-string:SCREEN-VALUE = "".
  ENABLE {&LIST-2} WITH FRAME {&FRAME-NAME}.
  APPLY "LEAVE" TO search-string IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Previous
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Previous C-Win
ON CHOOSE OF Btn_Previous IN FRAME DEFAULT-FRAME /* Prev */
DO:
  IF search-string NE "" THEN
  DO:
    ldummy = screen-report:SEARCH(search-string,18).
    IF NOT ldummy THEN
    MESSAGE "Search Match Not Found!" VIEW-AS ALERT-BOX INFORMATION.
    ELSE
    ldummy = screen-report:SET-SELECTION(screen-report:CURSOR-OFFSET,
         screen-report:CURSOR-OFFSET + LENGTH(search-string:SCREEN-VALUE)).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Print C-Win
ON CHOOSE OF Btn_Print IN FRAME DEFAULT-FRAME /* Print */
DO:
  DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
  DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
  DEFINE VARIABLE result AS LOGICAL NO-UNDO.
  
  SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
  IF NOT printok THEN
  RETURN NO-APPLY.

  /* Use Progress Print. Always use Font#9 in Registry (set above) */
  RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 9, INPUT 0, INPUT 0, INPUT 0, OUTPUT result).

/*
  OUTPUT TO PRINTER.
  INPUT FROM VALUE(list-name) NO-ECHO.
  REPEAT:
    IMPORT UNFORMATTED list-text.
    IF ASC(SUBSTR(list-text,1,1)) = 12 THEN
    DO:
      PAGE.
      list-text = SUBSTR(list-text,2).
    END.
    IF list-text NE "" THEN
    PUT UNFORMATTED list-text SKIP.
    ELSE
    PUT UNFORMATTED SKIP(1).
    list-text = "".
  END.
  INPUT CLOSE.
  OUTPUT CLOSE.
*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Save_As
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Save_As C-Win
ON CHOOSE OF Btn_Save_As IN FRAME DEFAULT-FRAME /* Save As */
DO:
  DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
  DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

  ASSIGN
    list-name = screen-file-name + IF screen-file-name NE "" THEN "rpt" ELSE ""
    init-dir = "users\" + USERID("NOSWEAT").
  SYSTEM-DIALOG GET-FILE list-name
      TITLE      "Enter Listing Name to SAVE AS ..."
      FILTERS    "Listing Files (*.rpt)" "*.rpt",
                 "All Files (*.*)" "*.*"
      INITIAL-DIR init-dir
      ASK-OVERWRITE
      CREATE-TEST-FILE
      SAVE-AS
      USE-FILENAME
      UPDATE OKpressed.
  IF NOT OKpressed THEN
  RETURN NO-APPLY.
  ASSIGN
    screen-file-name = SUBSTR(list-name,R-INDEX(list-name,"\") + 1,
        R-INDEX(list-name,".") - R-INDEX(list-name,"\"))
    ldummy = screen-report:SAVE-FILE(list-name)
    {&WINDOW-NAME}:TITLE = "Screen Viewer - '" + screen-file-name + "'".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Search C-Win
ON CHOOSE OF Btn_Search IN FRAME DEFAULT-FRAME /* Search */
DO:
  ENABLE search-string WITH FRAME {&FRAME-NAME}.
  APPLY "ENTRY" TO search-string.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME search-string
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL search-string C-Win
ON LEAVE OF search-string IN FRAME DEFAULT-FRAME
DO:
  {&SELF-NAME} = {&SELF-NAME}:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
  DISABLE {&SELF-NAME} WITH FRAME {&FRAME-NAME}.
  ldummy = screen-report:CLEAR-SELECTION().
  IF {&SELF-NAME} NE "" THEN
  DO:
    ldummy = screen-report:SEARCH({&SELF-NAME},16).
    IF NOT ldummy THEN
    MESSAGE "Search Match Not Found!" VIEW-AS ALERT-BOX INFORMATION.
    ELSE
    DO:
      ldummy = screen-report:SET-SELECTION(screen-report:CURSOR-OFFSET -
           LENGTH(search-string:SCREEN-VALUE),screen-report:CURSOR-OFFSET).
      ENABLE {&LIST-1} WITH FRAME {&FRAME-NAME}.
    END.
  END.
  ELSE
  DISABLE {&LIST-1} WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL search-string C-Win
ON RETURN OF search-string IN FRAME DEFAULT-FRAME
DO:
  APPLY "LEAVE" TO {&SELF-NAME}.
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
  {methods/enhance.i}
  {methods/nowait.i}
  ASSIGN
    {&WINDOW-NAME}:TITLE = "Screen Viewer - '" + screen-file-name + "'"
    list-name = "users~/" + USERID("NOSWEAT") + "~/" + screen-file-name
    ldummy = screen-report:READ-FILE(list-name).
  IF NOT ldummy THEN
  APPLY "CHOOSE" TO Btn_Open IN FRAME {&FRAME-NAME}.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR "CLOSE" OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win _DEFAULT-ENABLE
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
  DISPLAY screen-report search-string 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE screen-report RECT-4 RECT-1 RECT-5 RECT-2 Btn_Open Btn_Print 
         Btn_Delete Btn_Save_As Btn_Search Btn_Font Btn_Close 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus C-Win 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/setfocus.i screen-report}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


