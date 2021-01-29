&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: config.w

  Description: System Configurations

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 2.28.1998
  Updated: 1.4.2019

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

DEFINE VARIABLE selected-name AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES config

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define FIELDS-IN-QUERY-DEFAULT-FRAME config.logs_dir ~
config.spool_dir config.taskName config.taskType config.taskDate ~
config.taskTime config.cueCard config.taskerHTMLFolder ~
config.taskerLastExecuted config.start_page_no 
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH config SHARE-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH config SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME config
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME config


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnUpdate btnClose 
&Scoped-Define DISPLAYED-FIELDS config.logs_dir config.spool_dir ~
config.taskName config.taskType config.taskDate config.taskTime ~
config.cueCard config.taskerHTMLFolder config.taskerLastExecuted ~
config.start_page_no 
&Scoped-define DISPLAYED-TABLES config
&Scoped-define FIRST-DISPLAYED-TABLE config


/* Custom List Definitions                                              */
/* configFields,List-2,List-3,List-4,List-5,F1                          */
&Scoped-define configFields config.logs_dir config.spool_dir ~
config.taskName config.taskType config.taskDate config.taskTime ~
config.cueCard config.taskerHTMLFolder config.start_page_no 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClose 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.png":U
     LABEL "&Close" 
     SIZE 8 BY 1.91.

DEFINE BUTTON btnUpdate 
     IMAGE-UP FILE "Graphics/32x32/navigate_check.png":U
     LABEL "&Update" 
     SIZE 8 BY 1.91.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 19 BY 2.38.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      config SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     config.logs_dir AT ROW 1.24 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 82 BY 1
          BGCOLOR 15 
     config.spool_dir AT ROW 2.43 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 82 BY 1
          BGCOLOR 15 
     config.taskName AT ROW 3.62 COL 24 WIDGET-ID 22
          VIEW-AS TOGGLE-BOX
          SIZE 15 BY 1
     config.taskType AT ROW 3.62 COL 40 WIDGET-ID 28
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY 1
     config.taskDate AT ROW 3.62 COL 56 WIDGET-ID 24
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY 1
     config.taskTime AT ROW 3.62 COL 72 WIDGET-ID 26
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY 1
     config.cueCard AT ROW 4.81 COL 24 WIDGET-ID 30
          VIEW-AS TOGGLE-BOX
          SIZE 24 BY 1
     config.taskerHTMLFolder AT ROW 6 COL 22 COLON-ALIGNED WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 62 BY 1
          BGCOLOR 15 
     btnUpdate AT ROW 6.24 COL 88 HELP
          "Update/Save System Configurations"
     btnClose AT ROW 6.24 COL 97 HELP
          "Cancel Update or Close Window"
     config.taskerLastExecuted AT ROW 7.19 COL 22 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
          BGCOLOR 15 
     config.start_page_no AT ROW 7.19 COL 79 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
          BGCOLOR 15 
     "Email Subject:" VIEW-AS TEXT
          SIZE 14 BY 1 AT ROW 3.62 COL 10 WIDGET-ID 32
     RECT-1 AT ROW 6 COL 87
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106 BY 7.62
         FGCOLOR 1 .


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
         TITLE              = "System Configurations"
         HEIGHT             = 7.62
         WIDTH              = 106
         MAX-HEIGHT         = 7.62
         MAX-WIDTH          = 106
         VIRTUAL-HEIGHT     = 7.62
         VIRTUAL-WIDTH      = 106
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
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
       btnUpdate:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".

/* SETTINGS FOR TOGGLE-BOX config.cueCard IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN config.logs_dir IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN config.spool_dir IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN config.start_page_no IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX config.taskDate IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN config.taskerHTMLFolder IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN config.taskerLastExecuted IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX config.taskName IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX config.taskTime IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX config.taskType IN FRAME DEFAULT-FRAME
   NO-ENABLE 1                                                          */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "ASI.config"
     _Query            is OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* System Configurations */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* System Configurations */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClose C-Win
ON CHOOSE OF btnClose IN FRAME DEFAULT-FRAME /* Close */
DO:
    IF {&SELF-NAME}:LABEL EQ "&Close" THEN
        APPLY "CLOSE" TO THIS-PROCEDURE.
    ELSE DO WITH FRAME {&FRAME-NAME}:
        DISABLE {&configFields}.
        ASSIGN
            {&SELF-NAME}:LABEL = "&Close"
            btnUpdate:LABEL    = "&Update"
            .
        RUN enable_UI.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUpdate C-Win
ON CHOOSE OF btnUpdate IN FRAME DEFAULT-FRAME /* Update */
DO:
    IF {&SELF-NAME}:LABEL = "&Update" THEN DO WITH FRAME {&FRAME-NAME}:
        ENABLE {&configFields}.
        ASSIGN
            {&SELF-NAME}:LABEL = "&Save"
            btnClose:LABEL     = "&Cancel"
            .
        APPLY "ENTRY" TO config.logs_dir.
    END.
    ELSE DO WITH FRAME {&FRAME-NAME}:
        DISABLE {&configFields}.
        ASSIGN
            {&SELF-NAME}:LABEL = "&Update"
            btnClose:LABEL     = "&Close"
            .
        ASSIGN {&configFields}.
        IF config.cueCard THEN DO TRANSACTION:
            FIND FIRST cueCardText EXCLUSIVE-LOCK
                 WHERE cueCardText.cueID     EQ 0
                   AND cueCardText.cueTextID EQ 0
                   AND cueCardText.cueType   EQ "Message"
                 NO-ERROR.
            IF AVAILABLE cueCardText THEN
            cueCardText.isActive = YES.
            ELSE DO:
                CREATE cueCardText.
                ASSIGN
                    cueCardText.cueID     = 0
                    cueCardText.cueTextID = 0
                    cueCardText.cueType   = "Message"
                    .
            END. /* else */
            RELEASE cueCardText.
        END. /* if cuecard */
        ELSE DO TRANSACTION:
            FOR EACH cueCardText EXCLUSIVE-LOCK
                WHERE cueCardText.cueID     EQ 0
                  AND cueCardText.cueTextID EQ 0
                  AND cueCardText.cueType   EQ "Message"
                :
                IF cueCardText.cueOrder EQ 0 THEN
                cueCardText.isActive = NO.
                ELSE
                DELETE cueCardText.
            END. /* each cuecardtext */
            FOR EACH xCueCard EXCLUSIVE-LOCK
                WHERE xCueCard.cueType EQ "Message"
                :
                DELETE xCueCard.
            END. /* each xcuecard */
        END. /* do trans */
    END. /* else do */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME config.logs_dir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL config.logs_dir C-Win
ON HELP OF config.logs_dir IN FRAME DEFAULT-FRAME /* Logs Directory */
DO:
    {methods/folderLookup.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME config.spool_dir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL config.spool_dir C-Win
ON HELP OF config.spool_dir IN FRAME DEFAULT-FRAME /* Spool Directory */
DO:
    {methods/folderLookup.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME config.taskerHTMLFolder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL config.taskerHTMLFolder C-Win
ON HELP OF config.taskerHTMLFolder IN FRAME DEFAULT-FRAME /* Tasker HTML Folder */
DO:
    {methods/folderLookup.i}
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
  {methods/nowait.i}
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

  {&OPEN-QUERY-DEFAULT-FRAME}
  GET FIRST DEFAULT-FRAME.
  IF AVAILABLE config THEN 
    DISPLAY config.logs_dir config.spool_dir config.taskName config.taskType 
          config.taskDate config.taskTime config.cueCard config.taskerHTMLFolder 
          config.taskerLastExecuted config.start_page_no 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnUpdate btnClose 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

