&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: spool.w

  Description: Spool Requests

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 03/08/98

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

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-3 RECT-1 spool_list Btn_Process ~
Btn_Delete Btn_Close 
&Scoped-Define DISPLAYED-OBJECTS spool_list spool_title spool_date ~
spool_time user_id 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 spool_list Btn_Process Btn_Delete 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Close 
     LABEL "&Close" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Delete 
     LABEL "&Delete" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Process 
     LABEL "&Process" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE spool_date AS DATE FORMAT "99/99/9999":U 
     LABEL "Date" 
      VIEW-AS TEXT 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE spool_time AS CHARACTER FORMAT "X(256)":U 
     LABEL "Time" 
      VIEW-AS TEXT 
     SIZE 13 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE spool_title AS CHARACTER FORMAT "X(256)":U 
     LABEL "Title" 
      VIEW-AS TEXT 
     SIZE 42 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE user_id AS CHARACTER FORMAT "X(256)":U 
     LABEL "User ID" 
      VIEW-AS TEXT 
     SIZE 17 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 54 BY 13.81.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 54 BY 5.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 54 BY 1.67.

DEFINE VARIABLE spool_list AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 42 BY 12.95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     spool_list AT ROW 1.95 COL 12 HELP
          "Select Spool Request" NO-LABEL
     Btn_Process AT ROW 21.24 COL 3 HELP
          "Process Spool Requests"
     Btn_Delete AT ROW 21.24 COL 19 HELP
          "DELETE Selected Spool Request"
     Btn_Close AT ROW 21.24 COL 40 HELP
          "CLOSE Spool Requests"
     spool_title AT ROW 16.48 COL 11 COLON-ALIGNED
     spool_date AT ROW 17.91 COL 11 COLON-ALIGNED
     spool_time AT ROW 17.91 COL 40 COLON-ALIGNED
     user_id AT ROW 19.33 COL 11 COLON-ALIGNED
     RECT-2 AT ROW 15.76 COL 2
     RECT-3 AT ROW 21 COL 2
     "Spool Requests" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 1.24 COL 3
     RECT-1 AT ROW 1.48 COL 2
     "Spool Request Detail" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 15.52 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 56 BY 22
         FONT 6.


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
         TITLE              = "Spool Requests"
         HEIGHT             = 22
         WIDTH              = 56
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 56
         VIRTUAL-HEIGHT     = 22
         VIRTUAL-WIDTH      = 56
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
ASSIGN
       Btn_Close:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


ASSIGN
       Btn_Delete:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


ASSIGN
       Btn_Process:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


/* SETTINGS FOR BUTTON Btn_Delete IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Process IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN spool_date IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR SELECTION-LIST spool_list IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR FILL-IN spool_time IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN spool_title IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN user_id IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME






/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Spool Requests */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Spool Requests */
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
  MESSAGE "Delete '" + spool_title:SCREEN-VALUE + "'?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
          UPDATE OKdelete AS LOGICAL.
  IF NOT OKdelete THEN
  RETURN NO-APPLY.
  OS-DELETE VALUE(config.spool_dir + "/" + spool_title:SCREEN-VALUE + ".spl").
  OS-DELETE VALUE(config.spool_dir + "/" + spool_title:SCREEN-VALUE + ".rpt").
  RUN Get-Spool-Requests.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Process C-Win
ON CHOOSE OF Btn_Process IN FRAME DEFAULT-FRAME /* Process */
DO:
  RUN Get_Procedure IN Persistent-Handle ("chkspool.",OUTPUT run-proc,yes).
  RUN Get-Spool-Requests.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME spool_list
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spool_list C-Win
ON VALUE-CHANGED OF spool_list IN FRAME DEFAULT-FRAME
DO:
  RUN Spool-Details.
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

ON ESC OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  FIND FIRST config NO-LOCK.
  RUN Get-Spool-Requests.
  {methods/nowait.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
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
  DISPLAY spool_list spool_title spool_date spool_time user_id 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-2 RECT-3 RECT-1 spool_list Btn_Process Btn_Delete Btn_Close 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-Spool-Requests C-Win 
PROCEDURE Get-Spool-Requests :
/*------------------------------------------------------------------------------
  Purpose:     Get Spool Requests
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE search-dir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE file-name AS CHARACTER FORMAT "X(26)" NO-UNDO.
  DEFINE VARIABLE attr-list AS CHARACTER FORMAT "X(4)" NO-UNDO.
  DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
  DEFINE VARIABLE cdummy AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE ctitle AS CHARACTER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      spool_list:LIST-ITEMS = ""
      spool_title:SCREEN-VALUE = ""
      spool_date:SCREEN-VALUE = ""
      spool_time:SCREEN-VALUE = ""
      user_id:SCREEN-VALUE = ""
      search-dir = config.spool_dir.
  END.
  INPUT FROM OS-DIR(search-dir) NO-ECHO.
  REPEAT:
    SET file-name ^ attr-list.
    IF attr-list NE "f" OR
       INDEX(file-name,".spl") = 0 THEN
    NEXT.
    cdummy = cdummy + file-name + ",".
  END.
  INPUT CLOSE.
  IF cdummy = "" THEN
  DO:
    DISABLE {&LIST-1} WITH FRAME {&FRAME-NAME}.
    MESSAGE "No Spool Requests Exist!" VIEW-AS ALERT-BOX INFORMATION.
    RETURN.
  END.
  DO i = 1 TO NUM-ENTRIES(cdummy) - 1:
    INPUT FROM VALUE(config.spool_dir + "/" + ENTRY(i,cdummy)) NO-ECHO.
    IMPORT ctitle.
    ldummy = spool_list:ADD-LAST(ctitle) IN FRAME {&FRAME-NAME}.
    INPUT CLOSE.
  END.
  DO WITH FRAME {&FRAME-NAME}:
    spool_list:SCREEN-VALUE = spool_list:ENTRY(1).
    APPLY "VALUE-CHANGED" TO spool_list.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Spool-Details C-Win 
PROCEDURE Spool-Details :
/*------------------------------------------------------------------------------
  Purpose:     Get Spool Details for selected spool request.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE spoolfile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ctitle AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cuserid AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cdate AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ctime AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ampm AS CHARACTER NO-UNDO.

  spoolfile = config.spool_dir + "/" + spool_list:SCREEN-VALUE IN FRAME {&FRAME-NAME} + ".spl".
  INPUT FROM VALUE(spoolfile) NO-ECHO.
  IMPORT ctitle cuserid.
  IMPORT ^.
  IMPORT ^.
  IMPORT cdate.
  IMPORT ctime.
  IMPORT ampm.
  INPUT CLOSE.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      spool_title:SCREEN-VALUE = ctitle
      spool_date:SCREEN-VALUE = cdate
      spool_time:SCREEN-VALUE = ctime + " " + ampm
      user_id:SCREEN-VALUE = cuserid.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


