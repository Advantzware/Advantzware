&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          jobs             PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: touch/postjobs.w

  Description: Post Completed Jobs

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 05.30.2000

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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES jobmach

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH jobmach SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME jobmach
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME jobmach


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS machines Btn_Unselect_All Btn_Select_All ~
Btn_Post Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS machines 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Btn_Unselect_All Btn_Select_All Btn_Post 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel DEFAULT 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Post 
     LABEL "&Post Selections" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Btn_Select_All 
     LABEL "&Select All" 
     SIZE 16 BY 1.14.

DEFINE BUTTON Btn_Unselect_All 
     LABEL "&Unselect All" 
     SIZE 16 BY 1.14.

DEFINE VARIABLE machines AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE NO-DRAG SORT SCROLLBAR-VERTICAL 
     SIZE 44 BY 20.24 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      jobmach SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     machines AT ROW 1.71 COL 2 HELP
          "Select Machine(s) from Completed Jobs" NO-LABEL
     Btn_Unselect_All AT ROW 1.71 COL 46 HELP
          "Select All Machines"
     Btn_Select_All AT ROW 4.1 COL 46 HELP
          "Unselect All Machines"
     Btn_Post AT ROW 6.48 COL 46 HELP
          "Process Selected Machines Completed Jobs"
     Btn_Cancel AT ROW 20.52 COL 47 HELP
          "Cancel/Close this Window"
     "Machines from Completed Jobs" VIEW-AS TEXT
          SIZE 30 BY .62 AT ROW 1 COL 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 62 BY 21
         DEFAULT-BUTTON Btn_Cancel.


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
         TITLE              = "Post Completed Machine/Jobs"
         HEIGHT             = 21
         WIDTH              = 62
         MAX-HEIGHT         = 21
         MAX-WIDTH          = 62
         VIRTUAL-HEIGHT     = 21
         VIRTUAL-WIDTH      = 62
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


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR BUTTON Btn_Post IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Select_All IN FRAME DEFAULT-FRAME
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Unselect_All IN FRAME DEFAULT-FRAME
   1                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "jobs.jobmach"
     _Query            is OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Post Completed Machine/Jobs */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Post Completed Machine/Jobs */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Post
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Post C-Win
ON CHOOSE OF Btn_Post IN FRAME DEFAULT-FRAME /* Post Selections */
DO:
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE mach-list AS CHARACTER NO-UNDO.
  DEFINE VARIABLE mach-entry AS CHARACTER NO-UNDO.

  IF machines:SCREEN-VALUE = ? THEN
  DO:
    MESSAGE 'No Machine has been selected to Process!' VIEW-AS ALERT-BOX.
    RETURN NO-APPLY.
  END.
  DO i = 1 TO machines:NUM-ITEMS:
    IF machines:IS-SELECTED(i) THEN
    ASSIGN
      mach-entry = SUBSTR(machines:ENTRY(i),1,INDEX(machines:ENTRY(i),'(') - 2)
      mach-list = IF mach-list = '' THEN mach-entry
                  ELSE mach-list + ',' + mach-entry.
  END.
  RUN Get_Procedure IN Persistent-Handle ('post.',OUTPUT run-proc,no).
  IF run-proc NE '' THEN
  RUN VALUE(run-proc) (mach-list,Persistent-Handle).
  RUN Get_Machines.
  Btn_Cancel:LABEL = '&Close'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Select_All
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Select_All C-Win
ON CHOOSE OF Btn_Select_All IN FRAME DEFAULT-FRAME /* Select All */
DO:
  machines:SCREEN-VALUE = machines:LIST-ITEMS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Unselect_All
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Unselect_All C-Win
ON CHOOSE OF Btn_Unselect_All IN FRAME DEFAULT-FRAME /* Unselect All */
DO:
  machines:SCREEN-VALUE = ''.
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
  RUN Get_Machines.
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

  {&OPEN-QUERY-DEFAULT-FRAME}
  GET FIRST DEFAULT-FRAME.
  DISPLAY machines 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE machines Btn_Unselect_All Btn_Select_All Btn_Post Btn_Cancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_Machines C-Win 
PROCEDURE Get_Machines :
/*------------------------------------------------------------------------------
  Purpose:     populate mach-list with machines from completed jobs.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE mach-list AS CHARACTER NO-UNDO.

  FOR EACH cmpltjob NO-LOCK WHERE cmpltjob.posted = FALSE:
    IF INDEX(mach-list,cmpltjob.machine) NE 0 THEN
    NEXT.
    mach-list = IF mach-list = '' THEN cmpltjob.machine
                ELSE mach-list + ',' + cmpltjob.machine.
    FIND mach WHERE mach.m-code = cmpltjob.machine NO-LOCK NO-ERROR.
    IF AVAILABLE mach THEN
    mach-list = mach-list + ' (' + mach.m-dscr + ')'.
  END.
  IF mach-list = '' THEN
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      mach-list = ',,,,,         NO MACHINES AVAILABLE'
      Btn_Cancel:LABEL = '&Close'.
    DISABLE {&LIST-1}.
  END.
  machines:LIST-ITEMS IN FRAME {&FRAME-NAME} = mach-list.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


