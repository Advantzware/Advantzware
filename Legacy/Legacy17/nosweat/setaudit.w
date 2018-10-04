&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: setaudit.w

  Description: Set Audit Table Selections

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 07/29/98

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

DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS audit-tables db-names Btn_Audit_All ~
table-names Btn_Remove Btn_Add Btn_Tables_All Btn_Cancel Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS audit-tables db-names table-names 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Add 
     LABEL "&Add >>" 
     SIZE 16 BY 1.24.

DEFINE BUTTON Btn_Audit_All 
     LABEL "&Select All >" 
     SIZE 16 BY 1.24.

DEFINE BUTTON Btn_Cancel DEFAULT 
     LABEL "&Cancel" 
     SIZE 16 BY 1.24
     BGCOLOR 8 FONT 4.

DEFINE BUTTON Btn_OK AUTO-GO DEFAULT 
     LABEL "&OK" 
     SIZE 16 BY 1.24
     BGCOLOR 8 FONT 4.

DEFINE BUTTON Btn_Remove 
     LABEL "<< &Remove" 
     SIZE 16 BY 1.19.

DEFINE BUTTON Btn_Tables_All 
     LABEL "< Selec&t All" 
     SIZE 16 BY 1.24.

DEFINE VARIABLE audit-tables AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SORT SCROLLBAR-VERTICAL 
     SIZE 24 BY 22.38 NO-UNDO.

DEFINE VARIABLE db-names AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 24 BY 5.62 NO-UNDO.

DEFINE VARIABLE table-names AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 24 BY 15.52 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     audit-tables AT ROW 2.19 COL 43 HELP
          "Select Table Name" NO-LABEL
     db-names AT ROW 2.24 COL 1 HELP
          "Select Database Name" NO-LABEL
     Btn_Audit_All AT ROW 4.1 COL 26 HELP
          "Select All Tables to Audit"
     table-names AT ROW 9.05 COL 1 HELP
          "Select Table Name" NO-LABEL
     Btn_Remove AT ROW 9.81 COL 26 HELP
          "Remove Selected Table from Tables to Audit"
     Btn_Add AT ROW 11.48 COL 26 HELP
          "Add Selected Table to Tables to Audit"
     Btn_Tables_All AT ROW 16.95 COL 26 HELP
          "Select All Tables"
     Btn_Cancel AT ROW 21.48 COL 26 HELP
          "Use this function to CANCEL field selecition"
     Btn_OK AT ROW 23.14 COL 26 HELP
          "Use this function to ACCEPT selected field"
     "Table Names" VIEW-AS TEXT
          SIZE 15.6 BY 1 AT ROW 7.91 COL 4
          FONT 6
     "Tables to Audit" VIEW-AS TEXT
          SIZE 18 BY 1 AT ROW 1 COL 45
          FONT 6
     "Database Names" VIEW-AS TEXT
          SIZE 20 BY 1 AT ROW 1 COL 3
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 67 BY 24.


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
         TITLE              = "Audit Table Selections"
         HEIGHT             = 24
         WIDTH              = 67
         MAX-HEIGHT         = 24
         MAX-WIDTH          = 67
         VIRTUAL-HEIGHT     = 24
         VIRTUAL-WIDTH      = 67
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Audit Table Selections */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Audit Table Selections */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add C-Win
ON CHOOSE OF Btn_Add IN FRAME DEFAULT-FRAME /* Add >> */
DO:
  DO i = 1 TO table-names:NUM-ITEMS WITH FRAME {&FRAME-NAME}:
    IF table-names:IS-SELECTED(i) AND
      (NOT CAN-DO(audit-tables:LIST-ITEMS,table-names:ENTRY(i)) OR
       audit-tables:NUM-ITEMS = 0) THEN
    ldummy = audit-tables:ADD-LAST(table-names:ENTRY(i)).
  END.
  table-names:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Audit_All
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Audit_All C-Win
ON CHOOSE OF Btn_Audit_All IN FRAME DEFAULT-FRAME /* Select All > */
DO:
  audit-tables:SCREEN-VALUE IN FRAME {&FRAME-NAME} = audit-tables:LIST-ITEMS.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  FIND FIRST config EXCLUSIVE-LOCK.
  config.audit_tables = audit-tables:LIST-ITEMS IN FRAME {&FRAME-NAME}.
  APPLY "CLOSE" TO THIS-PROCEDURE.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Remove C-Win
ON CHOOSE OF Btn_Remove IN FRAME DEFAULT-FRAME /* << Remove */
DO:
  DO i = audit-tables:NUM-ITEMS TO 1 BY -1 WITH FRAME {&FRAME-NAME}:
    IF audit-tables:IS-SELECTED(i) THEN
    ldummy = audit-tables:DELETE(i).
  END.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Tables_All
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Tables_All C-Win
ON CHOOSE OF Btn_Tables_All IN FRAME DEFAULT-FRAME /* < Select All */
DO:
  table-names:SCREEN-VALUE IN FRAME {&FRAME-NAME} = table-names:LIST-ITEMS.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME db-names
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL db-names C-Win
ON VALUE-CHANGED OF db-names IN FRAME DEFAULT-FRAME
DO:
  RUN Get_Tables.
  table-names:SCREEN-VALUE = ENTRY(1,table-names:LIST-ITEMS).
  APPLY "VALUE-CHANGED" TO table-names.
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
  RUN Get_DBs.
  db-names:SCREEN-VALUE IN FRAME {&FRAME-NAME} =
      db-names:ENTRY(1) IN FRAME {&FRAME-NAME}.
  APPLY "VALUE-CHANGED" TO db-names.
  FIND FIRST config NO-LOCK NO-ERROR.
  IF NOT AVAILABLE config THEN
  RETURN.
  audit-tables:LIST-ITEMS IN FRAME {&FRAME-NAME} = config.audit_tables.
  {methods/nowait.i}
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
  DISPLAY audit-tables db-names table-names 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE audit-tables db-names Btn_Audit_All table-names Btn_Remove Btn_Add 
         Btn_Tables_All Btn_Cancel Btn_OK 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_DBs C-Win 
PROCEDURE Get_DBs :
/* -----------------------------------------------------------
  Purpose: Populate db-names selection list with connected database names
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE VARIABLE list-items AS CHARACTER NO-UNDO.

  RUN Get_Procedure IN Persistent-Handle (INPUT "db_list.",OUTPUT run-proc,no) NO-ERROR.
  IF run-proc NE "" THEN
  RUN VALUE(run-proc) (OUTPUT list-items).
  db-names:LIST-ITEMS IN FRAME {&FRAME-NAME} = list-items.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get_Tables C-Win 
PROCEDURE Get_Tables :
/* -----------------------------------------------------------
  Purpose: Populate table-names selection list with table names
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE VARIABLE list-items AS CHARACTER NO-UNDO.

  CREATE ALIAS dictdb FOR DATABASE VALUE(db-names:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  RUN Get_Procedure IN Persistent-Handle (INPUT "filelist.",OUTPUT run-proc,no) NO-ERROR.
  IF run-proc NE "" THEN
  RUN VALUE(run-proc) (OUTPUT list-items).
  table-names:LIST-ITEMS IN FRAME {&FRAME-NAME} = list-items.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus C-Win 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
  Purpose:     Set Focus
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/setfocus.i db-names}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

